(ns urdmi.plugin.ace
  (:require [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [urdmi.core :as api])
  (:import (java.io StringReader)
           (urdmi.core Project)))


(defn- build-bg-knowledge-file[plugin project])

(defn- update-knowledge-base-file[plugin project])

(defn- update-settings-file[plugin project]
  ())

(defn update-working-dir[this project changed-entry])

(defrecord AcePlugin []
  api/Plugin
  (new-project-creation-view ^api/View [this app-event-in-channel] )
  (run [this project]
    (let [ace-location "C:\\ML-Tools\\ACE-1.2.15\\windows\\bin\\ACE.exe"
          command "t\n"
          working-dir (api/get-working-dir project)]
      (shell/sh ace-location
                :in (StringReader. command)
                :dir working-dir
                ))
    )
  (update-working-dir [this project changed-entry])
  (rebuild-working-dir [this project])
  (new-entry-view ^api/View [this project entry to-app-channel]))

(defn create []
  (->AcePlugin))