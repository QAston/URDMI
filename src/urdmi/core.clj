(ns urdmi.core
  (:use clojure.pprint
        clojure.core.incubator)
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [urdmi.prolog :as prolog])
  (:import (java.io Writer Reader File)
           (java.nio.file Files CopyOption)
           (clojure.lang ISeq)))

(defrecord FileItem [data])

(defrecord DirItem [dir])

(defprotocol Plugin
  "All urdmi plugins must implement this protocol"
  (run [this project] "Run datamining engine associated to this plugin. Runs on a background thread, which can be interrupted to stop the job.")
  (generate-output [this project run-result] "Update output project directory with analysis of run-result. Runs on a background thread, which can be interrupted to stop the job.")
  (rebuild-working-dir [this project] "Rebuilds working directory of a datamining app. Runs on a background thread, which can be interrupted to stop the job.")
  (get-parser-context [this] "Returns parser-context object initialized for prolog engine used by the plugin.")

  (model-created [this project] "A hook on project creation. Should initialize model with default plugin settings. Returns ModelDiff object which is applied afterwards to the model.")
  (model-loaded [this project] "A hook on model loading. Returns ModelDiff object which is afterwards applied to the model.")
  (model-modified [this project key] "A hook on model modification. Returns ModelDiff object which is afterwards applied to the model.")
  )

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
(def working-dir-default-folder "working_dir")

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
  {relations-keyname get-relations-dir
   workdir-keyname   get-working-dir
   output-keyname    get-output-dir
   settings-keyname  get-settings-dir
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

(defn get-relation [^Project p [relname relarity :as rel]]
  (get-in p (model-map-keys relations-keyname (relation-to-filename rel))))

(defn get-settings-data [^Project p settings-filename]
  @(:data (get-in p (model-map-keys settings-keyname settings-filename))))

(defn get-relations [^Project p]
  (map second (get-in p (model-map-keys relations-keyname :dir))))

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
  (if diff
    (let [p (reduce set-model-item p (:set diff))
          p (reduce remove-model-item p (:remove diff))]
      p)
    p))

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