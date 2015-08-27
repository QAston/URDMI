(ns urdmi.app
  "stuff depending both on core and plugins namespace
  mainly plugin loading and app init."
  (:use urdmi.core)
  (:require [urdmi.plugin.ace :as ace]
            [urdmi.plugin.aleph :as aleph])
  (:import (urdmi.core App Project)
           (java.io File)))

(defn register-plugins [^App app]
  (-> app
      (register-plugin :ace #'ace/create)
      (register-plugin :aleph #'aleph/create)))

(defn load-project [^App app ^File dir]
  (let [app-with-project (load-settings (assoc app :project (base-project dir)))]
    (assoc app-with-project :project
                            (-> app-with-project
                                (:project)
                                (load-additions)
                                (load-relations)
                                (load-working-dir)
                                (load-output)))))

(defn init-app []
  (register-plugins (->App nil {}))
  )

(defn build-working-dir [^Project p]
  (rebuild-working-dir (:plugin p) p)
  )

(defn run-learning [^Project p]
  (run (:plugin p) p)
  )
