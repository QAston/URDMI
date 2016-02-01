(ns urdmi.core
  (:use clojure.pprint
        urdmi.util)
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
            [environ.core :refer [env]]
            [clojure.string :as string]
            [urdmi.prolog :as prolog])
  (:import (java.io Writer Reader File StringWriter)
           (java.nio.file Files CopyOption)
           (clojure.lang ISeq)))

(defn dev? []
  (= (env :urdmi-development) "true"))

(defrecord FileItem [data])

(defrecord DirItem [dir])

(defrecord RunResult [^String text ^boolean error?])

(defprotocol Plugin
  "All urdmi plugins must implement this protocol"
  (run ^urdmi.core.RunResult [this project] "Run datamining engine associated to this plugin.
    Runs on a background thread, which can be interrupted to stop the job.")
  (generate-output [this project ^urdmi.core.RunResult run-result]
    "Update output project directory with analysis of run-result.
    Runs on a background thread, which can be interrupted to stop the job.")
  (rebuild-working-dir [this project]
    "Rebuilds working directory of a datamining app.
    Runs on a background thread, which can be interrupted to stop the job.")
  (get-parser-context [this]
    "Returns parser-context object initialized for prolog engine used by the plugin.")

  (model-created [this project]
    "A hook on project creation.
    Should initialize model with default plugin settings.
    Returns ModelDiff object which is applied afterwards to the model.")
  (model-loaded [this project]
    "A hook on model loading.
    Returns ModelDiff object which is afterwards applied to the model.")
  (model-modified [this project key]
    "A hook on model modification.
    Returns ModelDiff object which is afterwards applied to the model.")
  (is-model-invalid [this project key]
    "A hook on model validation.
    Returns true if data for given key is not valid.")
  )

(defprotocol ContentPage
  (container-node [this]
    "Returns JavaFX node which will be attached to the application window")
  (show-data [this data data-key modified] "Called every time a page is shown.
  Data - project model, data-key - key in the model,
  modified - if model was modified from last call and redraw is needed")
  (read-data [this]
    "Returns model data for the key for which data is displayed"))

(defprotocol PluginGui
  (new-page [this project key >ui-requests]
    "Returns a view for editing/display of a menu entry"))

(defrecord Project [dir, project-dir, ^urdmi.core.Plugin plugin])

(defrecord App [^Project project ^clojure.lang.IPersistentMap plugins])

;set - seq of [model-key value] pairs to add/modify in the model
;remove- seq of model-keys to remove
(defrecord ModelDiff [^ISeq set ^ISeq remove])

(defn register-plugin [^App app name ^clojure.lang.IFn plugin]
  (assoc-in app [:plugins name] plugin))

(def relations-keyname :relations)
(def workdir-keyname :working-dir)
(def output-keyname :output)
(def settings-keyname :settings)
(def prolog-ext-keyname :prolog-ext)

(def prolog-ext-dir-name "prolog-ext")
(def settings-dir-name "settings")
(def output-dir-name "output")
(def relations-dir-name "relations")
(def working-dir-default-folder "build_dir")

(defn instant [data]
  "opposite of clojure.core/delay, because it's a fn, not a macro
  data - data to be stored as immediately-resolved delay"
  (delay data))

(defn move-file [^File src ^File dst]
  (Files/move (.toPath src) (.toPath dst) (into-array CopyOption [])))

(defn base-project [project-dir]
  (->Project {settings-keyname (map->DirItem {:name settings-keyname
                                              :dir  {"project.edn" (map->FileItem {:name "project.edn"
                                                                                   :data (instant {:working-dir (io/file working-dir-default-folder)})})}})}
             project-dir nil))
(defn model-map-keys
  "constructs a vector of keys into project :dir map from given node names (name-keys)"
  [& name-keys]
  (vec (dedupe (take (* 2 (count name-keys)) (interleave (repeat :dir) name-keys)))))

(defn to-item-key
  "constructs a vector of project node-names (model-keys) from a vector of keys into project :dir map"
  [dir-keys]
  (vec (remove (fn [k] (= :dir k)) dir-keys)))

(defn get-project-settings [project]
  @(:data (get (:dir (:settings (:dir project))) "project.edn")))

(defn get-working-dir [project]
  (let [wdir (:working-dir (get-project-settings project))]
    (if (fs/absolute? wdir)
      wdir
      (io/file (:project-dir project) wdir))))

(defn get-prolog-ext-dir [^Project p]
  (fs/file (:project-dir p) prolog-ext-dir-name))

(defn get-relations-dir [^Project p]
  (fs/file (:project-dir p) relations-dir-name))

(defn get-settings-dir [^Project p]
  (fs/file (:project-dir p) settings-dir-name))

(defn get-output-dir [^Project p]
  (fs/file (:project-dir p) output-dir-name))

(def ^:private model-path-to-file-map
  {relations-keyname  get-relations-dir
   workdir-keyname    get-working-dir
   output-keyname     get-output-dir
   settings-keyname   get-settings-dir
   prolog-ext-keyname get-prolog-ext-dir})

(defn item-key-to-file
  "Resolve name-keys to a java.io.File"
  ^File [^Project p item-key]

  (if (= item-key [])
    (fs/parent (item-key-to-file p [:settings]))
    (apply fs/file (keep identity (map (fn [element]
                                         (let [conv-fn (get model-path-to-file-map element)]
                                           (if conv-fn
                                             (conv-fn p)
                                             element))) item-key)))))

(import 'java.nio.file.Path)
(defn relativize-path [^File src ^File target]
  (let [^Path src-path (.toPath src)
        ^Path target-path (.toPath target)]
    (.toFile (.relativize src-path target-path))))

(defn file-to-item-key
  [^Project p ^File file]
  (let [key-dir-pairs (for [k [prolog-ext-keyname settings-keyname output-keyname workdir-keyname relations-keyname]]
                        [k (item-key-to-file p [k])])
        ]
    (first (mapcat (fn [[key ^File dir]]
                     (let [^Path file-path (.toPath file)
                           ^Path dir-path (.toPath dir)]
                       (when (.startsWith file-path dir-path)
                         (list (into [key] (fs/split (.toFile (.relativize dir-path file-path)))))
                         ))) key-dir-pairs))))

(defn file-model-branch? [m]
  (:dir m))

(defn file-model-children [m]
  (seq (map second (:dir m))))

(defn file-model-make-node [node children]
  (map->DirItem (assoc node :dir (into {} (map (fn [child]
                                                 [(:name child) child]) children)))))

(defn file-model-zipper [root]
  (zip/zipper file-model-branch? file-model-children file-model-make-node root))

(defn try-append-prolog-ext-file
  "appends file to the working directory file, creates files if not present"
  ([^Project p ^File prolog-ext-file-rel output]
   (try
     (let [file (io/file (get-prolog-ext-dir p) prolog-ext-file-rel)]
       (when (fs/file? file)
         (io/copy file output)))
     (catch Exception e))
    ))

(defn relation-to-filename [[relname relarity]]
  (str relname "_" relarity ".pl"))

(defn relation-to-string [[relname relarity]]
  (str relname "/" relarity))

(defn column-description-to-string [{:keys [name key]}]
  (str name (condp = key :primary " (PK)"
                         :foreign " (FK)"
                         "")))

(defn default-column-description [i]
  {:name (str "term_" i) :key :none})

(defn default-relation-column-description [arity]
  (with-meta (vec (map default-column-description (range arity))) {:default true}))

(defn cols-clause? [clause-ast]
  (and (= (:type clause-ast) :ast-functor)
       (= (first (:children clause-ast)) {:type :ast-atom, :name "urdmi_cols"})))

(defn parse-cols-clause [clause-ast]
  (vec (for [coldef (rest (:children clause-ast))]
         (let [name (:name (nth (:children coldef) 1))
               key (:name (nth (:children coldef) 2))]
           {:name name :key (keyword key)}))))

(defn get-relation [^Project p [relname relarity :as rel]]
  (get-in p (model-map-keys relations-keyname (relation-to-filename rel))))

(defn get-settings-data [^Project p settings-filename]
  @(:data (get-in p (model-map-keys settings-keyname settings-filename))))

(defn get-output-data [^Project p settings-filename]
  @(:data (get-in p (model-map-keys output-keyname settings-filename))))

(defn get-relations [^Project p]
  (map second (get-in p (model-map-keys relations-keyname :dir))))

(defn check-relation-term [^Project p [relation term :as relation-term-pair]]
  (and relation
       (get-relation p relation)
       term
       (< term (second relation))))

(defn thread-interruped? []
  (Thread/interrupted))

(defn dir-seq [^Project p dir-item-key]
  (let [root-dir (item-key-to-file p dir-item-key)
        dirs (fs/iterate-dir root-dir)
        ]
    (loop [dirs dirs result []]
      (if-not (seq dirs)
        result
        (let [[root subdirs files :as dir] (first dirs)]
          (recur (rest dirs) (into result (map #(fs/file root %) (concat files subdirs)))))
        ))))

(defn get-model-item [^Project p key]
  (get-in p (apply model-map-keys key)))

(defn file-item
  "Constructs a FileItem instance with in-memory contents"
  [data]
  (->FileItem (instant data)))

(defn set-model-item [^Project p [key item]]
  (let [item (assoc item :name (last key))
        p (if (and (not-empty (butlast key)) (not (get-in p (apply model-map-keys (butlast key)))))
            ; parent not found - add with parent
            (set-model-item p [(butlast key) (->DirItem item)])
            (assoc-in p (apply model-map-keys key) item)
            )]
    p))

(defn remove-model-item [^Project p key]
  (dissoc-in p (apply model-map-keys key)))

(defn apply-diff [^Project p ^ModelDiff diff]
  (if (and diff (instance? ModelDiff diff))
    (let [p (reduce set-model-item p (:set diff))
          p (reduce remove-model-item p (:remove diff))]
      p)
    p))

(defn merge-diff [^ModelDiff first-diff ^ModelDiff second-diff]
  (if (and first-diff second-diff (instance? ModelDiff first-diff) (instance? ModelDiff second-diff))

    (let [remove-from-set (set (concat (map first (:set second-diff))
                                       (:remove second-diff)))
          to-set (vec
                   ; set everything in second, and first, which were not removed/added in second
                   (concat
                     (->> (:set first-diff)
                          (remove (fn [[k v]]
                                     (remove-from-set k))))
                     (:set second-diff)
                     ))
          to-remove (vec
                      ; remove everything in second, and first, which were not added in second
                      (concat
                       (->> (:remove first-diff)
                            (remove (set (map first (:set second-diff)))))
                       (:remove second-diff)

                       ))]
      (->ModelDiff to-set to-remove))
    (if (and first-diff (instance? ModelDiff first-diff))
      first-diff
      second-diff)))

(defn resolve-executable-loc ^File [base-loc loc-string]
  (let [file (io/file loc-string)]
    (cond (fs/absolute? file) file
          (> (.getNameCount (.toPath file)) 1) (fs/file base-loc loc-string)
          true file
          )))

(defn resolve-relative-loc ^File [base-loc loc-string]
  (let [file (io/file loc-string)]
    (cond (fs/absolute? file) file
          true (fs/file base-loc loc-string))))

(defn get-all-relation-names [^Project p]
  (map :rel (get-relations p)))

(defn generate-relation-term-values-map [^Project p]
  (let [parser-context (get-parser-context (:plugin p))]
    (into {} (for [{:keys [data rel]} (get-relations p)]
               (let [asts @data
                     [name arity] rel]
                 [rel (into {} (for [i (range arity)]
                                 [i (set (map (fn [ast]
                                                (let [writer (StringWriter.)]
                                                  (prolog/pretty-print (nth (:children ast) (inc i)) parser-context writer)
                                                  (.toString writer))
                                                ) asts))]
                                 ))]
                 )))))

(defn generate-relation-column-map [^Project p]
  (let [parser-context (get-parser-context (:plugin p))]
    (into {} (for [{:keys [columns rel]} (get-relations p)]
               [rel columns]
               ))))

(defn generate-relation-columnname-map [^Project p]
  (into {}
        (map (fn [[rel cols]]
               [rel (mapv (fn [col] (column-description-to-string col)) cols)])
             (generate-relation-column-map p))))

(def ^String nl (System/getProperty "line.separator"))