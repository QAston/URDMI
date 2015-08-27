(ns urdmi.prolog-test
  (:use midje.sweet)
  (:import java.io.StringReader
           (com.ugos.jiprolog.engine PrologObject))
  (:require
    [urdmi.prolog :as prolog]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.zip :as zip]))

(fact "prolog ast hierarchy is defined"
      (isa? prolog/ast prolog/ast-cell prolog/ast-object) => true
      (isa? prolog/ast prolog/ast-functor prolog/ast-expression) => false
      (isa? prolog/ast prolog/ast-list prolog/ast-cell) => true)

(defn parse-string [context str]
  (prolog/prolog-sentence-seq context (StringReader. str)))

(fact "prolog parser produces ast nodes"
      (let [parser-context (prolog/ace-parser-context)]
        (parse-string parser-context "hello :- world.") => (list {:children (list {:name ":-", :type :ast-atom},
                                                                                  {:name "hello", :type :ast-atom},
                                                                                  {:name "world", :type :ast-atom})
                                                                  :type     :ast-functor})
        (parse-string parser-context "p(X).") => (list {:children (list {:name "p", :type :ast-atom}
                                                                        {:name "X", :type :ast-variable}),
                                                        :type     :ast-functor})
        (parse-string parser-context "'1as d'.") => (list {:name "1as d", :type :ast-atom})
        (parse-string parser-context "\"1as d\".") => (list {:value "1as d", :type :ast-string})
        (parse-string parser-context "q(+-\\Y).") => (list {:children (list {:name "q", :type :ast-atom}
                                                                            {:children (list {:name "+-\\", :type :ast-atom},
                                                                                             {:name "Y", :type :ast-variable}),
                                                                             :type     :ast-functor}),
                                                            :type     :ast-functor})
        (parse-string parser-context "ff([N0|T]).") => (list {:type     :ast-functor,
                                                              :children (list {:type :ast-atom, :name "ff"}
                                                                              {:type     :ast-list,
                                                                               :children (list {:type :ast-variable, :name "N0"},
                                                                                               {:type :ast-variable, :name "T" :rest true})})})
        (parse-string parser-context "ff([N0,T]).") => (list {:type     :ast-functor,
                                                              :children (list {:type :ast-atom, :name "ff"}
                                                                              {:type     :ast-list,
                                                                               :children (list {:type :ast-variable, :name "N0"},
                                                                                               {:type :ast-variable, :name "T"})})})))

(fact "prolog expr seq returns a seq of ast nodes"
      (let [parser-context (prolog/parser-context nil)
            empty (prolog/prolog-sentence-seq parser-context (StringReader. ""))
            single-sentence (prolog/prolog-sentence-seq parser-context (StringReader. "hello :- world."))
            two-sentences (prolog/prolog-sentence-seq parser-context (StringReader. "hello :- world. parent(a):- mother."))
            ]
        (count single-sentence) => 1
        (count two-sentences) => 2
        (count empty) => 0
        (isa? prolog/ast (:type (first two-sentences)) prolog/ast-object) => true
        )
      )

(fact "parsed expr has token position metadata - some have position, some dont :("
      (let [parser-context (prolog/parser-context nil)
            sentence (first (prolog/prolog-sentence-seq parser-context (StringReader. "hello :- world.")))
            op (first (prolog/prolog-sentence-seq parser-context (StringReader. "p([X|Y]).")))]
        (meta sentence) => {:column 5, :line 1, :position 6}
        (meta op) => {:column 1, :line 1, :position 2}
        ;(meta (last (:children (first (:children op))))) => {:line 1, :column 6, :position 7}
        ))

(defn get-prolog-files-for-tests [^String dir]
  (vec (flatten (for [extension ["pl" "kb" "bg" "b" "n" "f" "s"]]
                  (let [paths (map (memfn toPath) (file-seq (io/file dir)))]
                    (->> (filter #(.endsWith (str %) (str "." extension)) paths)
                         (map (fn [^java.nio.file.Path path]
                                (.subpath path 1 (.getNameCount path))))
                         (map str))
                    )))))

(fact "parser can parse example ace prolog files"
      (let [files (get-prolog-files-for-tests "dev-resources/ace_tilde")
            parser-context (prolog/ace-parser-context)]
        (doseq [file files]
          (fact file
                (try (count (prolog/prolog-sentence-seq parser-context
                                                        (io/make-reader
                                                          (io/resource file) {})))
                     (catch Exception e
                       (throw (Exception. (str "Couldn't parse " file) e)))) => anything)
          )))

(fact "parser can parse example aleph prolog files"
      (let [files (get-prolog-files-for-tests "dev-resources/aleph_default")
            parser-context (prolog/aleph-parser-context)]
        (doseq [file files]
          (fact file
                (try (count (prolog/prolog-sentence-seq parser-context
                                                        (io/make-reader
                                                          (io/resource file) {})))
                     (catch Exception e
                       (throw (Exception. (str "Couldn't parse " file) e)))) => anything)
          )))


(fact "pretty-print prints the same thing it got"
      (let [parser-context (prolog/parser-context
                             [(prolog/->Operator 200 "fy" "#")
                              (prolog/->Operator 200 "fy" "+")
                              (prolog/->Operator 200 "fy" "-")
                              (prolog/->Operator 200 "fy" "\\")
                              (prolog/->Operator 200 "fy" "+\\")
                              (prolog/->Operator 200 "fy" "-\\")
                              (prolog/->Operator 200 "yf" "+-")
                              (prolog/->Operator 200 "yf" "+-\\")])
            test-sentences ["\"1asd\"."
                            "\"1a\"\"sd\"."
                            "'1asd'."
                            "hello :- world."
                            "p(X)."
                            "p(+X)."
                            "q(Y+-)."
                            "q(-Y)."
                            "q(\\Y)."
                            "q(+\\Y)."
                            "q(-\\Y)."
                            "q(Y+-\\)."
                            "p(Y+X)."
                            "hasprop(fish, travel, swim)."
                            "hasproperty(Object, Property, Value)."
                            "isa(Object, Parent)."
                            "hasproperty(Parent, Property, Value)."
                            "hasproperty(Object, Property, Value) :- hasprop(Object, Property, Value)."
                            "hasproperty(Object) :- isa(Object), hasproperty(Parent)."
                            "frame(name(penguin), isa(bird), [color(brown),travel(walks)],[travel(walks)])."
                            "ff([N0,N1,N2|T], [N1|R]) :- isodd(N0), isodd(N2), !, ff([N1,N2|T], R)."
                            "ff(_, [])."
                            "find(L, R) :- append(L, [1,2], L1), ff([1|L], R)."
                            "type(X=X)."
                            "#(group, [7,8,9])."
                            "p(O) :- q(X), r(Y); z(W)."
                            "rmode(N: #(m*s*V : conj_constants(V), conj(V)))."
                            "rmode(N: #(const_generator(V):[vars=[V],models=m,subst=s,preprocessing=off],conj(V)))."
                            ]]
        (doseq [sentence test-sentences]
          (str/join (remove #{ \space \newline }
                            (prolog/pretty-print-sentences
                              parser-context
                              (parse-string parser-context sentence)))) => (str/join (remove #{\space \newline } sentence)))))

(fact "zipper queries are defined according to specs"
      (let [context (prolog/parser-context nil)
            atom-node {:name "+-\\", :type :ast-atom}
            list-node (first (parse-string context "ff([N0|T])"))
            predicate-node {:children (list {:name "p", :type :ast-atom}
                                            {:name "X", :type :ast-variable}),
                            :type     :ast-functor}]
        (prolog/ast-branch? atom-node) => false
        (prolog/ast-branch? list-node) => true
        (prolog/ast-branch? predicate-node) => true
        (prolog/ast-children list-node) => (list {:type :ast-atom, :name "ff"}
                                                 {:type :ast-list,
                                                  :children
                                                        [{:type :ast-variable, :name "N0"}
                                                         {:type :ast-variable, :name "T", :rest true}]})
        (prolog/ast-children predicate-node) => (list {:name "p", :type :ast-atom} {:name "X", :type :ast-variable})
        (:type (second (prolog/ast-children predicate-node))) => prolog/ast-variable
        ))

(fact "zipper ast-make-node adds a seq of children to given node"
      (prolog/ast-make-node {:children nil,
                             :type     :ast-list} nil)
      => {:children nil,
          :type     :ast-list}
      (prolog/ast-make-node {:children nil,
                             :type     :ast-list} (list {:name "p", :type :ast-atom}))
      => {:children (list {:name "p", :type :ast-atom}),
          :type     :ast-list}

      (prolog/ast-make-node {:children (list {:name "n", :type :ast-atom} {:name "o", :type :ast-atom}),
                             :type     :ast-list} (list {:name "p", :type :ast-atom}
                                                        {:name "q", :type :ast-atom}
                                                        {:name "r", :type :ast-atom}))
      => {:children (list {:name "p", :type :ast-atom}
                          {:name "q", :type :ast-atom}
                          {:name "r", :type :ast-atom}),
          :type     :ast-list})

(fact "zipper defined for seqs of ast nodes"
      (let [atoms (list {:name "p", :type :ast-atom} {:name "q", :type :ast-atom})
            empty (list)]
        (prolog/ast-branch? atoms) => true
        (prolog/ast-branch? empty) => true
        (prolog/ast-children atoms) => atoms
        (prolog/ast-children empty) => empty
        (prolog/ast-make-node empty (list {:name "r", :type :ast-atom})) => (list {:name "r", :type :ast-atom})
        (prolog/ast-make-node atoms (list {:name "r", :type :ast-atom})) => (list {:name "r", :type :ast-atom})
        (prolog/ast-make-node atoms (list {:name "r", :type :ast-atom} {:name "s", :type :ast-atom})) => (list {:name "r", :type :ast-atom} {:name "s", :type :ast-atom})
        ))

(facts prolog/quote-atom
       (fact "quotes a string for use in prolog atom (')"
             (prolog/quote-atom "asd") => "asd"
             (prolog/quote-atom "as\\d") => "as\\\\d"
             (prolog/quote-atom "as'd") => "as''d"
             (prolog/quote-atom "as\\'d") => "as\\\\''d"))

(facts prolog/quote-string
       (fact "quotes a string for use in prolog string (\")"
             (prolog/quote-string "asd") => "asd"
             (prolog/quote-string "as\\d") => "as\\\\d"
             (prolog/quote-string "as\"d") => "as\"\"d"
             (prolog/quote-string "as\\\"d") => "as\\\\\"\"d"))

(facts prolog/parse-single-sentence
       (let [context (prolog/parser-context[])]
         (fact "returns nil when more than single sentence"
               (prolog/parse-single-sentence context "a. b.") => nil)
         (fact "returns nil when more no empty string"
               (prolog/parse-single-sentence context "") => nil)
         (fact "returns nil when invalid sentence"
               (prolog/parse-single-sentence context ",") => nil)
         (fact "returns sentence when passed valid sentence"
               (prolog/parse-single-sentence context "a.") => {:name "a", :type :ast-atom})))

(facts prolog/parse-single-term
       (fact "parses a single prolog term, if not a single term returns nil"
             (let [context (prolog/parser-context [])]
               (prolog/parse-single-term context "") => nil
               (prolog/parse-single-term context ",") => nil
               (prolog/parse-single-term context "_") => {:name "_", :type :ast-variable}
               (prolog/parse-single-term context "a") => {:name "a", :type :ast-atom}
               (prolog/parse-single-term context "_,_") => nil
               (prolog/parse-single-term context "_,_,_") => nil
               (prolog/parse-single-term context "5") => {:type :ast-expression, :value 5}
               (prolog/parse-single-term context "a(b)") => {:children (list {:name "a", :type :ast-atom} {:name "b", :type :ast-atom}), :type :ast-functor}
               (prolog/parse-single-term context "5(r, v)") => nil
               (prolog/parse-single-term context "()") => nil
               (prolog/parse-single-term context "(1,2") => nil
               (prolog/parse-single-term context "1,2") => nil)))

(facts prolog/parse-single-atom
       (fact "accepts only a single, atom"
             (let [context (prolog/parser-context [])]
               (prolog/parse-single-atom context "") => nil
               (prolog/parse-single-atom context ",") => nil
               (prolog/parse-single-atom context "_") => nil
               (prolog/parse-single-atom context "_,_") => nil
               (prolog/parse-single-atom context "_,_,_") => nil
               (prolog/parse-single-atom context "5") => nil
               (prolog/parse-single-atom context "a(b)") => nil
               (prolog/parse-single-atom context "5(r, v)") => nil
               (prolog/parse-single-atom context "()") => nil
               (prolog/parse-single-atom context "(1,2") => nil
               (prolog/parse-single-atom context "1,2") => nil
               (prolog/parse-single-atom context "a") => {:name "a", :type :ast-atom}
               (prolog/parse-single-atom context "aTom") => {:name "aTom", :type :ast-atom})))