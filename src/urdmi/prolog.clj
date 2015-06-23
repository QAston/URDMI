(ns urdmi.prolog
  (:import (com.ugos.jiprolog.engine OperatorManager PrologParser JIPEngine ParserReader JIPDebugger PrologObject)
           (java.io Reader)
           (org.apache.commons.io.input ReaderInputStream)
           (com.ugos.io PushbackLineNumberInputStream)
           (java.nio.charset Charset)
           (clojure.lang ISeq)))

(set! JIPDebugger/debug true)

(defn ^:private prolog-expr-seq-impl [^PrologParser parser]
  (when-let [next (.parseNext parser)]
    (cons next (lazy-seq (prolog-expr-seq-impl parser)))))

(defrecord Operator [^int preference ^String type ^String name])

(defrecord ParserContext [^JIPEngine engine, ^OperatorManager op-manager])

(defn parser-context ^ParserContext [^ISeq operators]
  (let [jip-engine (JIPEngine.)
        op-manager (.getOperatorManager jip-engine)]
    (doseq [^Operator op operators]
      (.put op-manager (.preference op) (.type op) (.name op)))
    (->ParserContext jip-engine op-manager
      )))

(defn create-parser ^PrologParser [^ParserContext context, ^Reader rdr]
  (PrologParser. (ParserReader. (PushbackLineNumberInputStream. (ReaderInputStream. rdr (Charset/forName "US-ASCII"))))
                 (get context :op-manager)
                 (get context :engine)
                 "Input file"))

(defn aleph-parser-context []
  (parser-context [(->Operator 500 "fy" "#")
                   (->Operator 500 "fy" "*")
                   (->Operator 900 "xfy" "because")]))

(defn ace-parser-context []
  ;todo: these preferences made up
  (parser-context [(->Operator 200 "fy" "#")
                   (->Operator 200 "fy" "+")
                   (->Operator 200 "fy" "-")
                   (->Operator 200 "fy" "\\")
                   (->Operator 200 "fy" "+\\")
                   (->Operator 200 "fy" "-\\")
                   (->Operator 200 "fy" "+-")
                   (->Operator 200 "fy" "+-\\")]))

(defn prolog-sentence-seq
  "Lazily reads prolog expr from file, one at a time."
  ([^ParserContext context ^Reader rdr]
   (let [parser (create-parser context rdr)]
     (when-let [next (.parseNext parser)]
       (cons next (lazy-seq (prolog-expr-seq-impl parser)))))))
