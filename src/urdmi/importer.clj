(ns urdmi.importer
  (:require [urdmi.prolog :as prolog]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [urdmi.core :as core])
  (:import (urdmi.core Project)))

(defn- functor-filter [{:keys [type]}]
  (= type :ast-functor))

(defn- relation-sig [{:keys [children]}]
  [(:name (first children)) (dec (count children))]
  )

(defn- load-from-file
  [file parser-context]
  (try
    (let [relation-data-by-name (->> (prolog/prolog-sentence-seq parser-context (io/reader file))
                                     (filter functor-filter)
                                     (remove (fn [{:keys [type children]}]
                                               (= (:name (first children)) ":-")))
                                     (group-by relation-sig)
                                     (doall))]
      relation-data-by-name)
    (catch Exception e
      {})))

(defn- merge-relation-asts [ast1 ast2]
  (vec (distinct (concat ast1 ast2))))

(defn load-from-location
  "Returns imported relation data (dictionary relation->asts) from given location (data fetched recursively)."
  [file parser-context]
  (if (fs/file? file)
    (load-from-file file parser-context)
    (apply merge-with
           merge-relation-asts
           (for [[dir subdir-names file-names] (fs/iterate-dir file)
                 file-name file-names]
             (load-from-file (fs/file dir file-name) parser-context)))
    ))

(defn- load-from-project [^Project project]
  (let [pull-ast-data (fn [{:keys [data rel]}]
                        [rel @data]
                        )]
    (->> (core/get-relations project)
         (map pull-ast-data)
         (into {}))))

(defn- generate-import-data-diff [import-data]
  (let [replace-with (for [[[name arity :as rel] asts] import-data]
                       [[core/relations-keyname (core/relation-to-filename rel)]
                        (core/map->FileItem {:name (core/relation-to-filename rel)
                                             :rel  rel
                                             :data (core/instant asts)})])]
    (core/->ModelDiff replace-with [])))

(defn generate-model-diff-for-project-import
  "Return a model diff which imports data into the given project"
  [^Project project import-data]
  (let [project-data (load-from-project project)
        merged-data (merge-with merge-relation-asts project-data import-data)]
    (generate-import-data-diff merged-data))
  )


