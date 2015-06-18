(ns urdmi.prolog
  (:import (com.igormaznitsa.prologparser PrologParser PrologCharDataSource)
           (java.io Reader)))

(defn ^:private prolog-expr-seq-impl [^PrologParser parser]
  (when-let [next (.nextSentence parser)]
    (cons next (lazy-seq (prolog-expr-seq-impl parser)))))

(defn prolog-sentence-seq
  "Lazily reads prolog expr from file, one at a time."
  ([^Reader rdr]
   (let [parser (PrologParser. nil)
         input-adapter (PrologCharDataSource. rdr)]
     (when-let [next (.nextSentence parser input-adapter)]
       (cons next (lazy-seq (prolog-expr-seq-impl parser)))))))