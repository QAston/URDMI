(ns urdmi.plugin.aleph
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [urdmi.prolog :as prolog]
            [urdmi.core :as api]
            [clojure.zip :as zip])
  (:import (java.io StringReader)))

(use 'clojure.pprint)

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

    [(map second (get grouped-rel 1)) (map second (get grouped-rel 0))]))

(defn build-b-file [])
(defn build-f-file [])
(defn build-n-file [])

(defrecord AlephPlugin []
  api/Plugin
  (new-project-creation-view ^api/View [this app-event-in-channel])
  (run [this project]
    ;todo: plugin settings
    (let [plugin-settings nil #_(api/get-plugin-settings project)
          swiprolog-location (:swi-prolog-loc plugin-settings)
          aleph-location (:aleph-loc plugin-settings)
          working-dir (api/get-working-dir project)
          dbname (first (:target-rel plugin-settings))]
      (shell/sh swiprolog-location "-g" "true"
                :in (StringReader. (str "consult('" (prolog/quote aleph-location) "').\nread_all(" (prolog/quote dbname) ").\n.\ninduce.\nhalt.\n"))
                :dir working-dir
                )))
  (update-working-dir [this project changed-entry])
  (rebuild-working-dir [this project]
    (let [plugin-settings nil #_(api/get-plugin-settings project)
          working-dir (api/get-working-dir project)
          target-relation (:target-rel plugin-settings)
          target-relation-param (:target-rel-param plugin-settings)
          ]
      #_(io/output-stream )))
  (new-entry-view ^api/View [this project entry to-app-channel]))

(defn create []
  (->AlephPlugin))