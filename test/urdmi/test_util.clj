(ns urdmi.test-util
  (:require [urdmi.core :as core]))

(defn make-dummy-project [& key-data-paths]
  (-> (core/base-project "")
    (core/apply-diff (core/->ModelDiff key-data-paths []))))

(defn modify-dummy-project [p & key-data-paths]
  (core/apply-diff p (core/->ModelDiff key-data-paths [])))
