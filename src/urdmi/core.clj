(ns urdmi.core
  (:use clojure.pprint)
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [urdmi.prolog :as prolog])
  (:import (java.io Writer Reader)
           (java.nio.file Files CopyOption)))

(import 'java.io.File)

(defprotocol Plugin
  "All urdmi plugins must implement this protocol"
  (run [this project] "Run datamining engine associated to this plugin. Runs on a background thread, which can be interrupted to stop the job.")
  (generate-output [this project run-result] "Update output project directory with analysis of run-result. Runs on a background thread, which can be interrupted to stop the job.")
  (rebuild-working-dir [this project] "Rebuilds working directory of a datamining app. Runs on a background thread, which can be interrupted to stop the job.")
  (get-parser-context [this])
  )

(defrecord Project [dir, project-dir, ^urdmi.core.Plugin plugin])

(defrecord App [^Project project ^clojure.lang.IPersistentMap plugins])

(defn register-plugin [^App app name ^clojure.lang.IFn plugin]
  (assoc-in app [:plugins name] plugin))

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

(defn move-file [^File src ^File dst]
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

(import 'java.nio.file.Path)
(defn relativize-path [^File src ^File target]
  (let [^Path src-path (.toPath src)
        ^Path target-path (.toPath target)]
    (.toFile (.relativize src-path target-path))))

(defn file-to-name-keys
  [^Project p ^File file]
  (let [key-dir-pairs (for [k [additions-keyname settings-keyname output-keyname workdir-keyname relations-keyname]]
                        [k (name-keys-to-file p [k])])
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
  (assoc node :dir (into {} (map (fn [child]
                                   [(:name child) child]) children))))

(defn file-model-zipper [root]
  (zip/zipper file-model-branch? file-model-children file-model-make-node root))

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

(defn thread-interruped?[]
  (Thread/interrupted))

(defn dir-seq [^Project p dir-name-key]
  (let [root-dir (name-keys-to-file p dir-name-key)
        dirs (fs/iterate-dir root-dir)
        ]
    (loop [dirs dirs result []]
      (if-not (seq dirs)
        result
        (let [[root subdirs files :as dir] (first dirs)]
          (recur (rest dirs) (into result (map #(fs/file root %) (concat files subdirs)))))
        ))))