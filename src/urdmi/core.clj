(ns urdmi.core
  (:use clojure.pprint)
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [urdmi.prolog :as prolog])
  (:import (java.io Writer)
           (java.nio.file Files CopyOption)))

(import 'java.io.File)

(defprotocol Plugin
  "All urdmi plugins must implement this protocol"
  (run [this project] "Run datamining engine associated to this plugin. Updates output menu entries.")
  (rebuild-working-dir [this project] "Rebuilds working directory of a datamining app")
  )

(defrecord Project [dir, project-dir, ^urdmi.core.Plugin plugin])

(defrecord App [^Project project ^clojure.lang.IPersistentMap plugins])

(defn register-plugin [^App app name ^clojure.lang.IFn plugin]
  (assoc-in app [:plugins name] plugin))

(def project-keyname :project)
(def relations-keyname :relations)
(def workdir-keyname :working-dir)
(def output-keyname :output)
(def settings-keyname :settings)
(def additions-keyname :additions)

(def additions-dir-name "additions")
(def settings-dir-name "settings")
(def output-dir-name "output")
(def relations-dir-name "relations")
(def working-dir-default-folder "working_dir")

(defn move-file[^File src ^File dst]
  (Files/move (.toPath src) (.toPath dst) (into-array CopyOption [])))

(defn base-project [project-dir]
  (->Project {settings-keyname {:name settings-keyname
                                :dir
                                      {"project.edn" {:name "project.edn"
                                                      :data {:working-dir (io/file working-dir-default-folder)}}}}} project-dir nil))

(defn dir-keys
  "constructs a vector of keys into project :dir map from given node names (name-keys)"
  [& name-keys]
  (vec (dedupe (take (* 2 (count name-keys)) (interleave (repeat :dir) name-keys)))))

(defn to-name-keys
  "constructs a vector of project node-names (model-keys) from a vector of keys into project :dir map"
  [dir-keys]
  (vec (remove (fn [k] (= :dir k)) dir-keys)))

(defn get-project-settings [project]
  (:data (get (:dir (:settings (:dir project))) "project.edn")))

(defn get-working-dir [project]
  (let [wdir (:working-dir (get-project-settings project))]
    (if (fs/absolute? wdir)
      wdir
      (io/file (:project-dir project) wdir))))

(defn get-additions-dir [^Project p]
  (fs/file (:project-dir p) additions-dir-name))

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
   additions-keyname get-additions-dir})

(defn name-keys-to-file
  "Resolve name-keys to a java.io.File"
  ^File [^Project p name-keys]
  (apply fs/file (keep identity (map (fn [element]
                                       (let [conv-fn (get model-path-to-file-map element)]
                                         (if conv-fn
                                           (conv-fn p)
                                           element))) name-keys))))

#_(defn file-to-name-keys
  [])

(defn file-model-branch? [m]
  (:dir m))

(defn file-model-children [m]
  (map second (:dir m)))

(defn file-model-make-node [node children]
  (assoc node :dir (into {} (map (fn [child]
                                   [(:name child) child]) children))))

(defn file-model-zipper [root]
  (zip/zipper file-model-branch? file-model-children file-model-make-node root))

(import 'java.nio.file.Path)
(defn relativize-path [^File src ^File target]
  (let [^Path src-path (.toPath src)
        ^Path target-path (.toPath target)]
    (.toFile (.relativize src-path target-path))))

(defn iterate-subdir [subdir]
  (map #(clojure.core/update % 0 (fn [dirname]
                                   (io/file "." (relativize-path subdir dirname)))) (fs/iterate-dir subdir)))

(defn read-edn-data [^File file]
  (with-open [i (io/reader file)]
    (edn/read (java.io.PushbackReader. i))))

(defn append-addition
  "appends addition file to the working directory file, creates files if not present"
  ([^Project p ^File addition-file-rel ^Writer writer]
   (let [file (io/file (get-additions-dir p) addition-file-rel)]
     (when (fs/file? file)
       (with-open [file-rdr (io/reader file)]
         (loop []
           (let [data (.read file-rdr)]
             (when-not (== data -1)
               (.write writer data)
               (recur)))))))))

(defn save-entry [^Project p name-keys]
  )

(defn load-entry [^Project p name-keys]
  )

(defn- generate-model-map
  [dir root-node-name]
  (let [subdir-files (map
                       (fn [subdir]
                         (let [path-parts (vec (iterator-seq (.iterator (.toPath (first subdir)))))]
                           [path-parts (nth subdir 2)]))
                       (iterate-subdir dir))

        subdir-map (loop [files subdir-files res {}]
                     (if (seq files)
                       (let [[subdir-path file-set] (first files)
                             tree-path (take (* 2 (count subdir-path)) (interleave (mapv (memfn toString) subdir-path) (repeat :dir)))
                             files-in-dir (into {} (map (fn [filename] [filename {:name filename}]) file-set))
                             ]
                         (recur (rest files) (assoc-in (assoc-in res tree-path files-in-dir)
                                                       (conj (vec (butlast tree-path)) :name) (.toString (last subdir-path)))))
                       res
                       ))
        ]
    (assoc (get subdir-map ".") :name root-node-name)))

(defn load-working-dir [^Project p]
  (assoc-in p (dir-keys workdir-keyname) (generate-model-map (get-working-dir p) workdir-keyname)))

(defn load-additions [^Project p]
  (assoc-in p (dir-keys additions-keyname) (generate-model-map (get-additions-dir p) additions-keyname)))

(defn zipiter-to-name-keys [zipiter]
  (into (list (:name (zip/node zipiter))) (map :name (zip/path zipiter))))

(defn- read-edn-data-into-iter-node [^Project p zipiter node]
  (zip/replace zipiter (assoc node :data (read-edn-data (name-keys-to-file p (zipiter-to-name-keys zipiter))))))

(defn load-files [^Project p]
  (assoc-in p (dir-keys settings-keyname)
            (zip/root (loop [zipiter (file-model-zipper (get-in p (dir-keys settings-keyname)))]
                        (if (zip/end? zipiter)
                          zipiter
                          (let [node (zip/node zipiter)]
                            (if (:dir node)
                              (recur (zip/next zipiter))
                              (recur (zip/next (read-edn-data-into-iter-node p zipiter node))))
                            )
                          )
                        ))))

;todo: add update variants which work with diffs to filesystem

(defn deserialize-project-edn [data]
  (update data :working-dir (fn [workdir]
                              (io/file workdir))))

(defn serialize-project-edn [data]
  (update data :working-dir (fn [workdir-file]
                              (.toString workdir-file))))

(defn- load-plugin [^App app]
  {:pre [(not (nil? (:project app)))]}
  (let [plugin-key (:active-plugin (get-project-settings (:project app)))
        plugin-map (:plugins app)
        app (assoc-in app [:project :plugin] ((get plugin-map plugin-key (fn [] nil))))]
    app))

(defn- load-project-edn [^App app]
  (let [p (:project app)
        proj-settings-keys [settings-keyname "project.edn"]]
    (assoc-in app
              (cons :project (conj (apply dir-keys proj-settings-keys) :data))
              (deserialize-project-edn (read-edn-data (name-keys-to-file p proj-settings-keys))))))

(defn load-settings [^App app]
  (let [p (:project app)
        proj-with-settings (assoc-in p (dir-keys settings-keyname) (generate-model-map (get-settings-dir p) settings-keyname))
        proj-with-settings (load-files proj-with-settings)
        ]
    (-> app
        (assoc :project proj-with-settings)
        (load-project-edn)
        (load-plugin))))

(defn load-output [^Project p]
  (assoc-in p (dir-keys output-keyname) (generate-model-map (get-output-dir p) output-keyname)))

(defn load-relations [^Project p]
  (let [toread (fs/glob (get-relations-dir p) "*.pl")
        parser-context (prolog/parser-context nil)]
    (assoc-in p (dir-keys relations-keyname)
              {:dir (into {} (map (fn [file] (let [base-name (fs/base-name file ".pl")
                                                   name (string/join (butlast (string/split base-name #"_")))
                                                   arity (Integer/valueOf ^String (last (string/split base-name #"_")))
                                                   filename (.getName file)
                                                   asts (with-open [rel-rdr (io/reader file)]
                                                          (doall (prolog/prolog-sentence-seq parser-context rel-rdr)))]
                                               [filename {:name filename
                                                          :rel  [name arity]
                                                          :ast  asts}]))
                                  toread))})))

(defn relation-to-filename [[relname relarity]]
  (str relname "_" relarity ".pl"))

(defn relation-to-string [[relname relarity]]
  (str relname "/" relarity))

(defn get-relation-data [^Project p [relname relarity :as rel]]
  (get-in p (dir-keys relations-keyname (relation-to-filename rel))))

(defn get-settings-data [^Project p settings-filename]
  (:data (get-in p (dir-keys settings-keyname settings-filename))))

(defn get-relations [^Project p]
  (map second (get-in p (dir-keys relations-keyname :dir))))

(defn load-project [^App app ^File dir]
  (let [app-with-project (load-settings (assoc app :project (base-project dir)))]
    (assoc app-with-project :project
                            (-> app-with-project
                                (:project)
                                (load-additions)
                                (load-relations)
                                (load-working-dir)
                                (load-output)))))

(def entries-to-displaynames {
                              [project-keyname]                   "Project"
                              [project-keyname relations-keyname] "Relations"
                              [project-keyname workdir-keyname]   "Working Dir"
                              [output-keyname workdir-keyname]    "Output"
                              [settings-keyname workdir-keyname]  "Settings"
                              [additions-keyname workdir-keyname] "Additions"
                              })

(defn load-base-project[^File dir]
  (->
    (base-project dir)
    load-additions
    load-output
    load-working-dir
    load-relations))

(defn- subdir-map-to-vec [subdir-entry]
  (let [dirname (first subdir-entry)
        files (second subdir-entry)
        ]
    (vec (cons dirname
               (for [file files]
                 (subdir-map-to-vec file))))))

(defn model-map-to-menu-entries [m]
  )

