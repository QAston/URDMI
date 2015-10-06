(ns urdmi.plugin.aleph.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [urdmi.prolog :as prolog]
            [urdmi.core :as api]
            [clojure.zip :as zip]
            [urdmi.core :as core])
  (:import (java.io StringReader)
           (urdmi.core Project)))

(use 'clojure.pprint)

(def settings-filename "aleph.edn")

(defn split-by-relation-arg [rel-asts rel-arg]
  (let [grouped-rel (->> rel-asts
                         (map (fn [ast]
                                (let [child-idx rel-arg
                                      arg (:value (nth (:children ast) (inc child-idx)))
                                      newrel (update-in ast [:children] (fn [children]
                                                                          (->> children
                                                                               (map-indexed vector)
                                                                               (remove #(= (first %) child-idx))
                                                                               (map second)
                                                                               )))]
                                  [arg newrel])))
                         (group-by first))]

    [(sort-by hash (map second (get grouped-rel 1))) (sort-by hash (map second (get grouped-rel 0)))]))

(defn get-training-examples [^Project project]
  (let [plugin-settings (api/get-settings-data project settings-filename)
        target-relation (:target-rel plugin-settings)
        target-relation-param (:target-rel-param plugin-settings)
        target-rel-asts @(:data (api/get-relation project target-relation))
        ]
    (split-by-relation-arg target-rel-asts target-relation-param)
  ))

(defn get-db-name [^Project p]
  (first (:target-rel (api/get-settings-data p settings-filename))))

(defn build-b-file [plugin ^Project project]
  (let [
        parser-context (core/get-parser-context plugin)
        plugin-settings (api/get-settings-data project settings-filename)
        working-dir (api/get-working-dir project)
        background-relations (sort (vec (disj (set (map :rel (api/get-relations project))) (:target-rel plugin-settings))))
        filename (str (get-db-name project) ".b")
        ]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (doseq [rel background-relations]
        (let [ast @(:data (api/get-relation project rel))]
          (.append writer (str "\n % relation: "(api/relation-to-string rel) "\n"))
          (prolog/pretty-print-sentences parser-context ast writer)
          ))
      (api/try-append-addition project (io/file "bg_and_settings") writer))))

(defn build-f-file [plugin ^Project project]
  (let [working-dir (api/get-working-dir project)
        asts (first (get-training-examples project))
        parser-context (core/get-parser-context plugin)
        filename (str (get-db-name project) ".f")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context asts writer)
      (api/try-append-addition project (io/file "positive") writer))))

(defn build-n-file [plugin ^Project project]
  (let [working-dir (api/get-working-dir project)
        asts (second (get-training-examples project))
        parser-context (core/get-parser-context plugin)
        filename (str (get-db-name project) ".n")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context asts writer)
      (api/try-append-addition project (io/file "negative") writer))))

(defrecord AlephPlugin [parser-context]
  api/Plugin
  (run [this project]
    (let [plugin-settings (api/get-settings-data project settings-filename)
          swiprolog-location (:swi-prolog-loc plugin-settings)
          aleph-location (:aleph-loc plugin-settings)
          working-dir (api/get-working-dir project)
          dbname (get-db-name project)]
      (shell/sh swiprolog-location "-g" "true"
                :in (StringReader. (str "consult('" (prolog/quote-atom aleph-location) "').\nread_all(" (prolog/quote-atom dbname) ").\n.\ninduce.\nhalt.\n"))
                :dir working-dir
                )))
  (rebuild-working-dir [this project]
    (build-b-file this project)
    (build-f-file this project)
    (build-n-file this project))
  (get-parser-context [this]
    parser-context
    )
  (generate-output [this project run-result])
  (model-created [this project]
    (core/->ModelDiff [[[:additions "negative"] (core/file-item "%negative examples \n% file appended to the generated .n file")]
                       [[:additions "positive"] (core/file-item "%positive examples \n% file appended to the generated .f file")]
                       [[:additions "bg_and_settings"] (core/file-item "%background knowledge and aleph settings \n% file appended to the generated .b file")]
                       [[:settings settings-filename] (core/file-item
                                                        {:target-rel     nil
                                                        :aleph-loc      ""
                                                        :swi-prolog-loc ""
                                                        })]] []))
  (model-loaded [this project])
  (model-modified [this project key]))

(defn create []
  (->AlephPlugin (prolog/aleph-parser-context)))