(ns urdmi.app
  "stuff depending both on core and plugins namespace
  mainly plugin loading and app init."
  (:use urdmi.core)
  (:require [urdmi.plugin.ace :as ace]
            [urdmi.plugin.aleph :as aleph])
  (:import (urdmi.core App)))

(defn register-plugins [^App app]
  (-> app
      (register-plugin :ace #'ace/create)
      (register-plugin :aleph #'aleph/create)))

(defn init-app []
  (register-plugins (->App nil {}))
  )
