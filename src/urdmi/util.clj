(ns urdmi.util
  (:import (java.util.function Predicate)))

(defmacro ldef
  "Form working exactly like let, but defining globals instead of locals"
  [locals & exprs]
  `(do
     ~@(mapv (fn [name-value]
               `(def ~(symbol (first name-value)) ~(second name-value))) (partition 2 locals))
     ~@exprs))

(defn args-vec [& args]
  (vec args))

(defn find-thing [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (assoc m k newmap)
        )
      m)
    (dissoc m k)))

(defn predicate [pred-fn]
  (reify Predicate
    (test [this param]
      (pred-fn param)
      )))
