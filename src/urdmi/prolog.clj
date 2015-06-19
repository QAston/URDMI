(ns urdmi.prolog
  (:import (com.ugos.jiprolog.engine OperatorManager PrologParser JIPEngine ParserReader JIPDebugger)
           (java.io Reader)
           (org.apache.commons.io.input ReaderInputStream)
           (com.ugos.io PushbackLineNumberInputStream)
           (java.nio.charset Charset)))

(set! JIPDebugger/debug true)

(defn ^:private prolog-expr-seq-impl [^PrologParser parser]
  (when-let [next (.parseNext parser)]
    (cons next (lazy-seq (prolog-expr-seq-impl parser)))))

#_(defn prolog-sentence-seq
  "Lazily reads prolog expr from file, one at a time."
  ([^Reader rdr]
   (let [parser (PrologParser. nil)
         input-adapter (PrologCharDataSource. rdr)]
     (when-let [next (.nextSentence parser input-adapter)]
       (cons next (lazy-seq (prolog-expr-seq-impl parser)))))))

(defn- create-parser[^Reader rdr]
  (PrologParser. (ParserReader. (PushbackLineNumberInputStream. (ReaderInputStream. rdr (Charset/forName "US-ASCII"))))
                 (OperatorManager.)
                 (JIPEngine.)
                 "Input file"))

(defn prolog-sentence-seq
  "Lazily reads prolog expr from file, one at a time."
  ([^Reader rdr]
   (let [parser (create-parser rdr)]
     (when-let [next (.parseNext parser)]
       (cons next (lazy-seq (prolog-expr-seq-impl parser)))))))