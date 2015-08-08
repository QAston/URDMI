(ns urdmi.util)

(defmacro ldef
  "Form working exactly like let, but defining globals instead of locals"
  [locals & exprs]
  `(do
     ~@(mapv (fn [name-value]
               `(def ~(symbol (first name-value)) ~(second name-value))) (partition 2 locals))
     ~@exprs))

(defn args-vec[& args]
  (vec args))
