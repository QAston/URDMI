(ns urdmi.plugin.ace.core
  (:require [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [urdmi.core :as api]
            [urdmi.prolog :as prolog]
            [urdmi.core :as core])
  (:import (java.io StringReader)
           (urdmi.core Project)))

(def settings-filename "ace.edn")

(defn get-app-name [^Project p]
  (first (:target-rel (api/get-settings-data p settings-filename))))


(defn- build-bg-knowledge-file [plugin project]
  (let [
        parser-context (core/get-parser-context plugin)
        plugin-settings (api/get-settings-data project settings-filename)
        working-dir (api/get-working-dir project)
        background-relations (sort (vec (disj (set (map :rel (api/get-relations project))) (:target-rel plugin-settings))))
        filename (str (get-app-name project) ".bg")
        ]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (doseq [rel background-relations]
        (let [ast @(:data (api/get-relation project rel))]
          (.append writer (str "\n % relation: " (api/relation-to-string rel) "\n"))
          (prolog/pretty-print-sentences parser-context ast writer)
          ))
      (api/try-append-addition project (io/file "bg.pl") writer))))


(defn- build-knowledge-base-file [plugin project]
  (let [working-dir (api/get-working-dir project)
        plugin-settings (api/get-settings-data project settings-filename)
        target-rel-asts @(:data (api/get-relation project (:target-rel plugin-settings)))
        parser-context (core/get-parser-context plugin)
        filename (str (get-app-name project) ".kb")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context target-rel-asts writer)
      (api/try-append-addition project (io/file "kb.pl") writer))))

(defn- build-settings-file [plugin project]
  (let [working-dir (api/get-working-dir project)
        filename (str (get-app-name project) ".s")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (api/try-append-addition project (io/file "settings.pl") writer))))

(defrecord AcePlugin [parser-context]
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
    (build-settings-file this project))
  (get-parser-context [this]
    parser-context
    )
  (generate-output [this project run-result])
  (model-created [this project]
    (core/->ModelDiff [[[:additions "bg.pl"] (core/file-item "% background knowledge \n% file appended to the generated .bg file")]
                        [[:additions "kb.pl"] (core/file-item "% examples\n% file appended to the generated .kb file")]
                        [[:additions "settings.pl"] (core/file-item "% ace engine settings\n% file appended to the generated .s file")]
                       [[:settings settings-filename] (core/file-item {:target-rel nil
                                                                       :ace-loc    ""
                                                                       })]] []))
  (model-loaded [this project])
  (model-modified [this project key]))

(defn create []
  (->AcePlugin (prolog/ace-parser-context)))