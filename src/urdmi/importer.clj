(ns urdmi.importer
  (:require [urdmi.prolog :as prolog]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

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


(defn load-from-location
  "Returns imported relation data (dictionary relation->asts) from given location (data fetched recursively)."
  [file parser-context]
  (if (fs/file? file)
    (load-from-file file parser-context)
    (apply merge-with
           #(vec (distinct (concat %1 %2)))
           (for [[dir subdir-names file-names] (fs/iterate-dir file)
                 file-name file-names]
             (load-from-file (fs/file dir file-name) parser-context)))
    )
  )



(defn save-import
  "Appends imported data to relation files in given path"
  [relation-dir import-data])


