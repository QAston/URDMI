(ns urdmi.plugin.ace
  (:require [clojure.java.shell :as shell]
            [clojure.java.io :as io])
  (:import (java.io StringReader)))


(defn run [this project]
  (let [ace-location "C:\\ML-Tools\\ACE-1.2.15\\windows\\bin\\ACE.exe"
        command "t\n"
        working-dir (io/file (io/resource "projects/ace_tilde/working_dir"))]
    (shell/sh ace-location
                    :in (StringReader. command)
                    :dir working-dir
                    ))
  )

(defn- build-bg-knowledge-file[plugin project])

(defn- update-knowledge-base-file[plugin project])

(defn- update-settings-file[plugin project]
  ())

(defn update-working-dir[this project changed-entry])

