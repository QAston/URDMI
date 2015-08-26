(ns urdmi.plugin.ace
  (:require [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [urdmi.core :as api]
            [urdmi.prolog :as prolog])
  (:import (java.io StringReader)
           (urdmi.core Project)))

(def settings-filename "ace.edn")

(defn get-app-name [^Project p]
  (first (:target-rel (api/get-settings-data p settings-filename))))


(defn- build-bg-knowledge-file [plugin project]
  (let [
        parser-context (prolog/parser-context nil)
        plugin-settings (api/get-settings-data project settings-filename)
        working-dir (api/get-working-dir project)
        background-relations (sort (vec (disj (set (map :rel (api/get-relations project))) (:target-rel plugin-settings))))
        filename (str (get-app-name project) ".bg")
        ]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (doseq [rel background-relations]
        (let [ast (:ast (api/get-relation-data project rel))]
          (.append writer (str "\n % relation: " (api/relation-to-string rel) "\n"))
          (prolog/pretty-print-sentences parser-context ast writer)
          ))
      (api/append-addition project (io/file filename) writer))))


(defn- build-knowledge-base-file [plugin project]
  (let [working-dir (api/get-working-dir project)
        plugin-settings (api/get-settings-data project settings-filename)
        target-rel-asts (:ast (api/get-relation-data project (:target-rel plugin-settings)))
        parser-context (prolog/parser-context nil)
        filename (str (get-app-name project) ".kb")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context target-rel-asts writer)
      (api/append-addition project (io/file filename) writer))))

(defn- build-settings-file [plugin project]
  (let [working-dir (api/get-working-dir project)
        filename (str (get-app-name project) ".s")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (api/append-addition project (io/file filename) writer))))

(defrecord AcePlugin []
  api/Plugin
  (run [this project]
    (let [
          plugin-settings (api/get-settings-data project settings-filename)
          ace-location (:ace-loc plugin-settings)
          command "t\n"
          working-dir (api/get-working-dir project)]
      (shell/sh ace-location
                :in (StringReader. command)
                :dir working-dir
                ))
    )
  (rebuild-working-dir [this project]
    (build-bg-knowledge-file this project)
    (build-knowledge-base-file this project)
    (build-settings-file this project)))

(defn create []
  (->AcePlugin))