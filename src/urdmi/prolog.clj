(ns urdmi.prolog
  (:import (com.ugos.jiprolog.engine OperatorManager PrologParser JIPEngine ParserReader JIPDebugger PrologObject ConsCell Functor List Clause Atom Expression Variable)
           (java.io Reader)
           (org.apache.commons.io.input ReaderInputStream)
           (com.ugos.io PushbackLineNumberInputStream)
           (java.nio.charset Charset)
           (clojure.lang ISeq)))

(set! JIPDebugger/debug true)

(declare to-ast)

(defn ^:private prolog-expr-seq-impl [^PrologParser parser]
  (when-let [next (.parseNext parser)]
    (cons (to-ast next) (lazy-seq (prolog-expr-seq-impl parser)))))

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
     (when-let [^PrologObject next (.parseNext parser)]
       ;(println (.toString next (:engine context)))
       (cons (to-ast next) (lazy-seq (prolog-expr-seq-impl parser)))))))

(def ast-cell :ast-cell)
(def ast-object :ast-object)
(def ast-atom :ast-atom)
(def ast-expression :ast-expression)
(def ast-variable :ast-variable)
(def ast-functor :ast-functor)
(def ast-list :ast-list)
(def ast-clause :ast-clause)

(def ast
  (let [parent-to-child [
                         [ast-cell ast-functor]
                         [ast-cell ast-list]
                         [ast-cell ast-clause]
                         [ast-object ast-cell]
                         [ast-object ast-atom]
                         [ast-object ast-expression]
                         [ast-object ast-variable]]]

    (loop [h (make-hierarchy) mapping parent-to-child]
      (if-not (empty? mapping)
        (recur (derive h (second (first mapping)) (first (first mapping)))
               (rest mapping))
        h))
    ))

(defprotocol PAstConvertible
  (to-ast [^PrologObject obj])
  )

(defn- with-file-metadata [^PrologObject obj m]
  (with-meta m
             {:line         (.getLine obj)
              :column       (.getColumn obj)
              :get-position (.getPosition obj)}))

(extend-type Functor
  PAstConvertible
  (to-ast [^Functor obj]
    (with-file-metadata obj
                        {:type ast-functor
                         :head (assoc (to-ast (.getHead obj)) :name (.getFriendlyName obj))
                         :tail (to-ast (.getTail obj))})))

(defn get-functor-name
  "returns name for given functor ast node"
  [functor]
  {:pre  [(= (:type functor) ast-functor)]
   :post [(not= % nil)]}
  (:name (:head functor)))

(defn get-functor-arity
  "returns arity for given functor ast node"
  [functor]
  {:pre  [(= (:type functor) ast-functor)]
   :post [(not= % nil)]}
  (loop [head (:tail functor) i 0]
    (if head
      (recur (:tail head) (inc i))
      i)))

(extend-type List
  PAstConvertible
  (to-ast [^List obj]
    (with-file-metadata obj
                        {:type ast-list
                         :head (to-ast (.getHead obj))
                         :tail (to-ast (.getTail obj))})))

(extend-type Clause
  PAstConvertible
  (to-ast [^Clause obj]
    (with-file-metadata obj
                        {:type ast-clause
                         :head (to-ast (.getHead obj))
                         :tail (to-ast (.getTail obj))}
                        )))

(extend-type ConsCell
  PAstConvertible
  (to-ast [^Clause obj]
    (with-file-metadata obj
                        {:type ast-cell
                         :head (to-ast (.getHead obj))
                         :tail (to-ast (.getTail obj))}
                        )))

(extend-type Atom
  PAstConvertible
  (to-ast [^Atom obj]
    (with-file-metadata obj
                        {:type ast-atom
                         :name (.getName obj)}
                        )))

(extend-type Expression
  PAstConvertible
  (to-ast [^Expression obj]
    (with-file-metadata obj
                        {:type  ast-expression
                         :value (if (.isInteger obj) (int (.getValue obj))
                                                     (.getValue obj))}
                        )))

(extend-type Variable
  PAstConvertible
  (to-ast [^Variable obj]
    (with-file-metadata obj
                        {:type ast-variable
                         :name (if (= \^ (first (.getName obj)))
                                 "_"
                                 (.getName obj))}
                        )))

(extend-type nil
  PAstConvertible
  (to-ast [_]
    nil))


(defmulti pretty-print "Pretty prints a prolog ast node into builder."
          (fn [node op-manager ^StringBuilder builder] (:type node)))

(defn- pretty-print-params [obj op-manager ^StringBuilder builder]
  (loop [obj obj]
    (when-let [head (:head obj)]
      (pretty-print head, op-manager, builder)
      (let [tail (:tail obj)]
        (when (:head tail)
          (.append builder ","))
        (recur tail)))))

(defn- pretty-print-cons [obj op-manager ^StringBuilder builder]
  (loop [obj obj]
    (when-let [head (:head obj)]
      (pretty-print head, op-manager, builder)
      (let [tail (:tail obj)]
        (if (isa? ast (:type tail) ast-cell)
          (do
            (when (:head tail)
              (.append builder ","))
            (recur tail))
          (when (isa? ast (:type tail) ast-variable)
                    (do
                      (.append builder "|")
                      (pretty-print tail op-manager builder)))))
        )))

(defn- pretty-print-operator [functor ^com.ugos.jiprolog.engine.Operator op op-manager ^StringBuilder builder]
  (let [arity (get-functor-arity functor)
        args (:tail functor)]
    (cond
      (and (== arity 1) (.getPrefix op)) (do
                                           (pretty-print (:head functor) op-manager builder)
                                           (.append builder " ")
                                           (pretty-print (:head args) op-manager builder)
                                           )
      (and (== arity 1) (.getPostfix op)) (do
                                            (pretty-print (:head args) op-manager builder)
                                            (.append builder " ")
                                            (pretty-print (:head functor) op-manager builder)
                                            )
      :else (do
              (pretty-print (:head args) op-manager builder)
              (.append builder " ")
              (pretty-print (:head functor) op-manager builder)
              (.append builder " ")
              (let [next-arg (:tail args)
                    type (:type next-arg)]
                (if (or (isa? ast type ast-list)
                        (isa? ast type ast-functor)
                        (not (isa? ast type ast-cell)))
                  (pretty-print next-arg op-manager builder)
                  (pretty-print (:head next-arg) op-manager builder)))
              ))))

(defn is-operator
  "returns true when functor is found to be an operator in this ast node"
  [functor op-manager]
  (let [name (get-functor-name functor)]
    (when (.contains op-manager name)
      (let [^com.ugos.jiprolog.engine.Operator op (.get op-manager name)
            arity (get-functor-arity functor)]
        (when (or (and (== arity 1)
                       (or (.getPrefix op) (.getPostfix op)))
                  (and (== arity 2) (.getInfix op)))
          op
          )))))

(defmethod pretty-print ast-cell [obj op-manager ^StringBuilder builder]
  (pretty-print-cons obj op-manager builder))

(defmethod pretty-print ast-clause [obj op-manager ^StringBuilder builder]
  (pretty-print (:head obj) op-manager builder)
  (when-let [tail (:tail obj)]
    (.append builder ":-")
    (pretty-print (:head tail)))
  (.append builder ".")
  )

(defmethod pretty-print ast-functor [functor op-manager ^StringBuilder builder]
  (if-not (:tail functor)
    (pretty-print (:head functor))
    (if-let [op (is-operator functor op-manager)]
      (pretty-print-operator functor op op-manager builder)
      (let [params (:tail functor)]

          (do
            (pretty-print (:head functor) op-manager builder)
            (.append builder "(")
            (pretty-print-params params op-manager builder)
            (.append builder ")")))
        )))

(defmethod pretty-print ast-list [obj op-manager ^StringBuilder builder]
  (.append builder "[")
  (pretty-print-cons obj op-manager builder)
  (.append builder "]")
  )

(defmethod pretty-print ast-atom [obj _ ^StringBuilder builder]
  (.append builder (:name obj))
  )

(defmethod pretty-print ast-expression [obj _ ^StringBuilder builder]
  (.append builder (:value obj))
  )

(defmethod pretty-print ast-variable [obj _ ^StringBuilder builder]
  (.append builder (:name obj))
  )

(defmethod pretty-print nil [_ _ ^StringBuilder _]
  )

(defn pretty-print-sentences
  "Pretty-prints a a seq of prolog ast nodes. Assumes these are in order."
  [^ParserContext context, ^ISeq prolog-sentences]
  ;todo: handle seqs
  (let [builder (StringBuilder.)]
    (pretty-print (first prolog-sentences) (:op-manager context) builder)
    (.toString builder)))

