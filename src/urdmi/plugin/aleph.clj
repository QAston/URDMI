(ns urdmi.plugin.aleph
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [urdmi.prolog :as prolog]
            [urdmi.core :as api]
            [clojure.zip :as zip])
  (:import (java.io StringReader)
           (urdmi.core Project)))

(use 'clojure.pprint)

(def settings-filename "aleph.edn")

(defn split-by-relation-arg [rel-asts rel-arg]
  (let [grouped-rel (->> rel-asts
                         (map (fn [ast]
                                (let [child-idx rel-arg
                                      arg (:value (nth (:children ast) child-idx))
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
        target-rel-asts (:ast (api/get-relation-data project target-relation))
        ]
    (split-by-relation-arg target-rel-asts target-relation-param)
  ))

(defn get-db-name [^Project p]
  (first (:target-rel (api/get-settings-data p settings-filename))))

(defn build-b-file [^Project project]
  (let [
        parser-context (prolog/parser-context nil)
        plugin-settings (api/get-settings-data project settings-filename)
        working-dir (api/get-working-dir project)
        background-relations (sort (vec (disj (set (map :rel (api/get-relations project))) (:target-rel plugin-settings))))
        filename (str (get-db-name project) ".b")
        ]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (doseq [rel background-relations]
        (let [ast (:ast (api/get-relation-data project rel))]
          (.append writer (str "\n % relation: "(api/relation-to-string rel) "\n"))
          (prolog/pretty-print-sentences parser-context ast writer)
          ))
      (api/append-addition project (io/file filename) writer))))

(defn build-f-file [^Project project]
  (let [working-dir (api/get-working-dir project)
        asts (first (get-training-examples project))
        parser-context (prolog/parser-context nil)
        filename (str (get-db-name project) ".f")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context asts writer)
      (api/append-addition project (io/file filename) writer))))

(defn build-n-file [^Project project]
  (let [working-dir (api/get-working-dir project)
        asts (second (get-training-examples project))
        parser-context (prolog/parser-context nil)
        filename (str (get-db-name project) ".n")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context asts writer)
      (api/append-addition project (io/file filename) writer))))

(defrecord AlephPlugin []
  api/Plugin
  (new-project-creation-view ^api/View [this app-event-in-channel])
  (run [this project]
    (let [plugin-settings (api/get-settings-data project settings-filename)
          swiprolog-location (:swi-prolog-loc plugin-settings)
          aleph-location (:aleph-loc plugin-settings)
          working-dir (api/get-working-dir project)
          dbname (get-db-name project)]
      (shell/sh swiprolog-location "-g" "true"
                :in (StringReader. (str "consult('" (prolog/quote aleph-location) "').\nread_all(" (prolog/quote dbname) ").\n.\ninduce.\nhalt.\n"))
                :dir working-dir
                )))
  (update-working-dir [this project changed-name-keys])
  (rebuild-working-dir [this project]
    (build-b-file project)
    (build-f-file project)
    (build-n-file project))
  (new-entry-view ^api/View [this project entry to-app-channel]))

(defn create []
  (->AlephPlugin))