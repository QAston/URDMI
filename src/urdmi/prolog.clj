(ns urdmi.prolog
  (:import (com.ugos.jiprolog.engine OperatorManager PrologParser JIPEngine ParserReader JIPDebugger PrologObject ConsCell Functor List Clause Atom Expression Variable PString)
           (java.io Reader Writer StringWriter StringReader)
           (org.apache.commons.io.input ReaderInputStream)
           (com.ugos.io PushbackLineNumberInputStream)
           (java.nio.charset Charset)
           (clojure.lang ISeq))
  (:require [clojure.zip :as zip]
            [clojure.string :as string]))

(set! JIPDebugger/debug true)

(defrecord Operator [^int preference ^String type ^String name])

(defrecord ParserContext [^JIPEngine engine, ^OperatorManager op-manager])

(defn parser-context ^ParserContext [^ISeq operators]
  (let [jip-engine (JIPEngine.)
        op-manager (.getOperatorManager jip-engine)]
    (.setEnvVariable jip-engine "double_quotes" "chars")
    (doseq [^Operator op operators]
      (.put op-manager (.preference op) (.type op) (.name op)))
    (->ParserContext jip-engine op-manager
                     )))

(defn quote-atom
  "quotes string for use in prolog atom" [^String s]
  (.replace (.replace s "'" "''") "\\" "\\\\"))

(defn quote-string
  "quotes string for use in prolog strings" [^String s]
  (.replace (.replace s "\"" "\"\"") "\\" "\\\\"))

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

(def ast-cell "grouping together (parens) or grouping in :- functor (special case)" :ast-cell) ;
(def ast-object "all ast nodes have this type" :ast-object)
(def ast-atom "prolog atom" :ast-atom)
(def ast-expression "prolog expression" :ast-expression)
(def ast-variable "prolog variable" :ast-variable)
(def ast-functor "prolog functor or operator" :ast-functor)
(def ast-list "prolog list" :ast-list)
(def ast-string "prolog string" :ast-string)

(def ast
  (let [parent-to-child [
                         [ast-cell ast-functor]
                         [ast-cell ast-list]
                         [ast-object ast-cell]
                         [ast-object ast-atom]
                         [ast-object ast-expression]
                         [ast-expression ast-string]
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

(defn ^:private prolog-expr-seq-impl [^PrologParser parser]
  (when-let [next (.parseNext parser)]
    (cons (to-ast next) (lazy-seq (prolog-expr-seq-impl parser)))))

(defn prolog-sentence-seq
  "Lazily reads prolog expr from file, one at a time."
  ([^ParserContext context ^Reader rdr]
   (let [parser (create-parser context rdr)]
     (when-let [^PrologObject next (.parseNext parser)]
       (cons (to-ast next) (lazy-seq (prolog-expr-seq-impl parser)))))))

(defn parse-single-sentence
  "Returns a sentence, nil if more or empty"
  [^ParserContext context ^String sentence]
  (let [parser (create-parser context (StringReader. sentence))]
    (try
      (when-let [^PrologObject next (.parseNext parser)]
        (try
          ;returns nil when nothing more to parse
          (when-not (.parseNext parser)
            (to-ast next))
          (catch Exception e
            )))
      (catch Exception e
        ))))

(defn parse-single-term
  "Parses a term, returns nil if invalid or empty" [^ParserContext context ^String term]
  (let [sentence (parse-single-sentence context (str "term(" term ")."))]
    (try
      (let [terms (->> sentence
                       (:children)
                       (rest))]
        (when (= 1 (count terms))
          (first terms)))
      (catch Exception e
        ))))

(defn parse-single-atom
  "Parses an atom, returns nil if invalid or empty" [^ParserContext context ^String atom]
  (let [sentence (parse-single-sentence context (str atom "."))]
    (try
      (when (= :ast-atom (:type sentence))
        sentence)
      (catch Exception e
        ))))

(defn- with-file-metadata [^PrologObject obj m]
  (if (> (.getLine obj) 0)
    (with-meta m
               {:line     (.getLine obj)
                :column   (.getColumn obj)
                :position (.getPosition obj)})
    m))

(defn cell-children
  "Returns a seq of children from a recursive ConsCell list"
  [^ConsCell obj]
  (loop [^ConsCell cell obj children []]
    (if-let [head (.getHead cell)]
      (let [children (conj children (to-ast head))]
        (if-let [tail (.getTail cell)]
          (if (instance? ConsCell tail)
            (recur tail children)
            (conj children (assoc (to-ast tail) :rest true))
            )
          children
          ))
      children)))

(extend-type ConsCell
  PAstConvertible
  (to-ast [^ConsCell obj]
    (with-file-metadata obj
                        {:type     ast-cell
                         :children (cell-children obj)})))

(extend-type Functor
  PAstConvertible
  (to-ast [^Functor obj]
    (with-file-metadata obj
                        {:type     ast-functor
                         :children (let [tail-children (if-let [tail (.getTail obj)]
                                                         (cell-children tail)
                                                         '())
                                         name-node (assoc (to-ast (.getHead obj)) :name (.getFriendlyName obj))]
                                     (cons name-node tail-children))})))

(defn get-functor-name
  "returns name for given functor ast node"
  [functor]
  {:pre  [(= (:type functor) ast-functor)]
   :post [(not= % nil)]}
  (:name (first (:children functor))))

(defn get-functor-arity
  "returns arity for given functor ast node"
  [functor]
  {:pre  [(= (:type functor) ast-functor)]
   :post [(not= % nil)]}
  (count (rest (:children functor))))

(extend-type List
  PAstConvertible
  (to-ast [^List obj]
    (with-file-metadata obj
                        {:type     ast-list
                         :children (cell-children obj)})))

(extend-type Clause
  PAstConvertible
  (to-ast [^Clause obj]
    (throw (RuntimeException. "Clause is invalid node type, use functor instead!"))))

(extend-type Atom
  PAstConvertible
  (to-ast [^Atom obj]
    (with-file-metadata obj
                        {:type ast-atom
                         :name (.getName obj)}
                        )))

(extend-type PString
  PAstConvertible
  (to-ast [^PString obj]
    (with-file-metadata obj
                        {:type  ast-string
                         :value (.getString obj)}
                        )))

(extend-type Expression
  PAstConvertible
  (to-ast [^Expression obj]
    (with-file-metadata obj
                        {:type  ast-expression
                         :value (if (.isInteger obj) (long (.getValue obj))
                                                     (.getValue obj))}
                        )))

(extend-type Variable
  PAstConvertible
  (to-ast [^Variable obj]
    (with-file-metadata obj
                        {:type ast-variable
                         :name (if (= \^ (first (.getName obj)))
                                 "_"
                                 (.getName obj))
                         }
                        )))

(defn ast-branch?
  "returns true if given node can have children"
  [node]
  (if (seq? node)
    true
    (isa? ast (:type node) ast-cell)))

(defn ast-children
  "returns a seq of children of this cons-cell"
  [node]
  (if (seq? node)
    node
    (seq (:children node))))

(defn ast-make-node
  "given an existing node and a seq of
children, returns a new branch node with the supplied children.
root is the root node."
  [node children]
  {:pre [(ast-branch? node)]}
  (if (seq? node)
    (with-meta children (meta node))
    ; todo: handle auto-promoting nodes to cell when needed (operators)
    (assoc node :children children)))

(defn ast-zipper
  "returns a clojure.zip zipper for given root"
  [root]
  (zip/zipper ast-branch? ast-children ast-make-node root))


(defmulti pretty-print "Pretty prints a prolog ast node into builder."
          (fn [node parser-context ^Writer builder] (:type node)))

(defn- pretty-print-params [nodes-seq parser-context ^Writer builder]
  (loop [nodes nodes-seq]
    (when-let [head (first nodes)]
      (pretty-print head, parser-context, builder)
      (if-let [tail (seq (rest nodes))]
        (do
          (.append builder ",")
          (recur tail)))
      )))

(defn- pretty-print-cons [obj parser-context ^Writer builder]
  (loop [nodes (:children obj)]
    (when-let [head (first nodes)]
      (pretty-print head, parser-context, builder)
      (if-let [tail (seq (rest nodes))]
        (do
          (when-let [head (first tail)]
            (.append builder (if (:rest head) "|" ",")))
          (recur tail)))
      )))

(defn- pretty-print-operator [functor ^com.ugos.jiprolog.engine.Operator op parser-context ^Writer builder]
  (let [arity (get-functor-arity functor)
        head (first (:children functor))
        args (rest (:children functor))]
    (cond
      (and (== arity 1) (.getPrefix op)) (do
                                           (pretty-print head parser-context builder)
                                           (.append builder " ")
                                           (pretty-print (first args) parser-context builder)
                                           )
      (and (== arity 1) (.getPostfix op)) (do
                                            (pretty-print (first args) parser-context builder)
                                            (.append builder " ")
                                            (pretty-print head parser-context builder)
                                            )
      :else (do
              (pretty-print (first args) parser-context builder)
              (.append builder " ")
              (pretty-print head parser-context builder)
              (.append builder " ")
              (pretty-print (second args) parser-context builder)
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

(defmethod pretty-print ast-functor [functor parser-context ^Writer builder]
  (let [children (:children functor)
        head (first children)
        params (seq (rest children))]
    (if-not params
      (pretty-print head)
      (if-let [op (is-operator functor (:op-manager parser-context))]
        (pretty-print-operator functor op parser-context builder)
        (do
          (pretty-print head parser-context builder)
          (.append builder "(")
          (pretty-print-params params parser-context builder)
          (.append builder ")")))
      )))

(defmethod pretty-print ast-list [obj parser-context ^Writer builder]
  (.append builder "[")
  (pretty-print-cons obj parser-context builder)
  (.append builder "]")
  )

(defmethod pretty-print ast-string [obj parser-context ^Writer builder]
  (.append builder "\"")
  (.append builder ^String (quote-string (:value obj)))
  (.append builder "\"")
  )


(defmethod pretty-print ast-cell [obj parser-context ^Writer builder]
  ;todo: determine if parens should be printed or not based on operator manager
  ;take "parent operator" as a param, when "," priority bigger than parent-operator param don't print parens
  ;(.append builder "(")
  (pretty-print-cons obj parser-context builder)
  ;(.append builder ")")
  )

(defmethod pretty-print ast-atom [obj parser-context ^Writer builder]
  ; if atom cannot be parsed - quote it
  (if (parse-single-atom parser-context (:name obj))
    (.append builder ^String (:name obj))
    (.append builder ^String (str \' (quote-atom (:name obj)) \'))))

(defmethod pretty-print ast-expression [obj _ ^Writer builder]
  (.append builder ^String (.toString (:value obj)))
  )

(defmethod pretty-print ast-variable [obj _ ^Writer builder]
  (.append builder ^String (:name obj))
  )

(defn pretty-print-sentences
  "Pretty-prints a a seq of prolog ast nodes. Assumes these are in order."
  ([^ParserContext context, ^ISeq prolog-sentences ^Writer writer]
   (doseq [sentence prolog-sentences]
     (pretty-print sentence context writer)
     (binding [*out* writer]
       (.append writer \.)
       (newline))
     ))
  ([^ParserContext context, ^ISeq prolog-sentences]
   (let [writer (StringWriter.)]
     (pretty-print-sentences context prolog-sentences writer)
     (.toString writer)
     )))

(defn pretty-print-expression
  "Pretty-prints expression given as the ast"
  [^ParserContext parser-context, ast]
  (let [writer (StringWriter.)]
    (pretty-print ast parser-context writer)
    (.toString writer)))

(defn- remove-term [term-idx sentence]
  (let [sentence (update-in sentence [:children]
                            (fn [children]
                              (->> children
                                   (map-indexed vector)
                                   (remove #(= (first %) term-idx))
                                   (map second)
                                   )))]

    (if (= 1 (count (:children sentence)))
      (first (:children sentence))
      sentence)
    ))

(defn extract-relation-arg
  "returns relation gruped by given arg index, with relation having that arg removed"
  [rel-asts rel-term-idx]
  (let [child-idx (inc rel-term-idx)
        grouped-rel (->> rel-asts
                         (group-by (fn [ast]
                                     (nth (:children ast) child-idx)))
                         (map (fn [[arg asts]]
                                [arg (map (partial remove-term child-idx) asts)])))]
    (into {} grouped-rel)))