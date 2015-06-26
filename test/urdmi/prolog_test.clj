(ns urdmi.prolog-test
  (:use midje.sweet)
  (:import java.io.StringReader
           (com.ugos.jiprolog.engine PrologObject))
  (:require
    [urdmi.prolog :as prolog]
    [clojure.java.io :as io]
    [clojure.string :as str]))

(fact "prolog ast hierarchy is defined"
      (isa? prolog/ast prolog/ast-cell prolog/ast-object) => true
      (isa? prolog/ast prolog/ast-functor prolog/ast-expression) => false
      (isa? prolog/ast prolog/ast-list prolog/ast-cell) => true)

(defn parse-string [context str]
  (prolog/prolog-sentence-seq context (StringReader. str)))

(fact "prolog parser produces ast nodes"
      (let [parser-context (prolog/ace-parser-context)]
        (parse-string parser-context "hello :- world.") => (list {:head {:name ":-", :type :ast-atom},
                                                                  :tail
                                                                        {:head {:name "hello", :type :ast-atom},
                                                                         :tail
                                                                               {:head {:name "world", :type :ast-atom}, :tail nil, :type :ast-cell},
                                                                         :type :ast-cell},
                                                                  :type :ast-functor})
        (parse-string parser-context "p(X).") => (list {:head {:name "p", :type :ast-atom},
                                                        :tail
                                                              {:head {:name "X", :type :ast-variable},
                                                               :tail nil,
                                                               :type :ast-cell},
                                                        :type :ast-functor})
        (parse-string parser-context "q(+-\\Y).") => (list {:head {:name "q", :type :ast-atom},
                                                            :tail
                                                                  {:head
                                                                         {:head {:name "+-\\", :type :ast-atom},
                                                                          :tail
                                                                                {:head {:name "Y", :type :ast-variable},
                                                                                 :tail nil,
                                                                                 :type :ast-cell},
                                                                          :type :ast-functor},
                                                                   :tail nil,
                                                                   :type :ast-cell},
                                                            :type :ast-functor})
        (parse-string parser-context "ff([N0|T]).") => (list {:type :ast-functor,
                                                              :head {:type :ast-atom, :name "ff"},
                                                              :tail
                                                                    {:type :ast-cell,
                                                                     :head
                                                                           {:type :ast-list,
                                                                            :head {:type :ast-variable, :name "N0"},
                                                                            :tail {:type :ast-variable, :name "T"}},
                                                                     :tail nil}})
        (parse-string parser-context "ff([N0,T]).") => (list {:type :ast-functor,
                                                              :head {:type :ast-atom, :name "ff"},
                                                              :tail
                                                                    {:type :ast-cell,
                                                                     :head
                                                                           {:type :ast-list,
                                                                            :head {:type :ast-variable, :name "N0"},
                                                                            :tail
                                                                                  {:type :ast-list,
                                                                                   :head {:type :ast-variable, :name "T"},
                                                                                   :tail nil}},
                                                                     :tail nil}})))

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
        (meta (:head (:tail sentence))) => nil
        (meta (:head (:tail (:tail sentence)))) => nil
        (meta op) => {:column 1, :line 1, :position 2}
        (meta (:head (:head (:tail op)))) => nil
        (meta (:tail (:head (:tail op)))) => {:line 1, :column 6, :position 7}))

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
            test-sentences ["hello :- world."
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
                            "rmode(N: #(m*s*V : conj_constants(V), conj(V)))."
                            "rmode(N: #(const_generator(V):[vars=[V],models=m,subst=s,preprocessing=off],conj(V)))."
                            ]]
        (doseq [sentence test-sentences]
          (str/join (filter #(not= % \space)
                            (prolog/pretty-print-sentences
                              parser-context
                              (parse-string parser-context sentence)))) => (str/join (filter #(not= % \space) (butlast sentence))))))

(fact "zipper queries are defined according to specs"
      (let [context (prolog/parser-context nil)
            atom-node {:name "+-\\", :type :ast-atom}
            list-node (first (parse-string context "ff([N0,N1,N2|T], [N1|R])"))
            predicate-node {:head {:name "p", :type :ast-atom},
                            :tail
                                  {:head {:name "X", :type :ast-variable},
                                   :tail nil,
                                   :type :ast-cell},
                            :type :ast-functor}]
        (prolog/ast-branch? atom-node) => false
        (prolog/ast-branch? list-node) => true
        (prolog/ast-branch? predicate-node) => true
        (prolog/ast-children predicate-node) => (list {:name "p", :type :ast-atom} {:head {:name "X", :type :ast-variable}, :tail nil, :type :ast-cell})
        (:type (second (prolog/ast-children list-node))) => prolog/ast-cell
        ))

(fact "zipper ast-make-node adds a seq of children to given list node"
      (prolog/ast-make-node {:head nil,
                             :tail nil,
                             :type :ast-list} nil)
      => {:head nil,
          :tail nil,
          :type :ast-list}
      (prolog/ast-make-node {:head nil,
                             :tail nil,
                             :type :ast-list} (list {:name "p", :type :ast-atom}))
      => {:head {:name "p", :type :ast-atom},
          :tail nil,
          :type :ast-list}

      (prolog/ast-make-node {:head {:name "p", :type :ast-atom},
                             :tail nil,
                             :type :ast-list} (list {:name "q", :type :ast-atom}
                                                    {:name "r", :type :ast-atom}))
      => {:head {:name "p", :type :ast-atom},
          :tail {:head {:name "q", :type :ast-atom},
                 :tail {:head {:name "r", :type :ast-atom},
                        :tail nil,
                        :type :ast-list},
                 :type :ast-list},
          :type :ast-list}

      (prolog/ast-make-node {:head {:name "p", :type :ast-atom},
                             :tail {:name "r", :type :ast-atom},
                             :type :ast-list} (list {:name "q", :type :ast-atom}))

      => {:head {:name "p", :type :ast-atom},
          :tail {:head {:name "q", :type :ast-atom},
                 :tail {:name "r", :type :ast-atom},
                 :type :ast-list},
          :type :ast-list}

      (prolog/ast-make-node {:head nil,
                             :tail nil,
                             :type :ast-list} (list {:name "p", :type :ast-atom}
                                                    {:name "q", :type :ast-atom}
                                                    {:name "r", :type :ast-atom}))
      => {:head {:name "p", :type :ast-atom},
          :tail {:head {:name "q", :type :ast-atom},
                 :tail {:head {:name "r", :type :ast-atom},
                        :tail nil,
                        :type :ast-list},
                 :type :ast-list},
          :type :ast-list})

(fact "zipper ast-make-node adds a seq of children to given cell node"
      (prolog/ast-make-node {:head nil,
                             :tail nil,
                             :type :ast-cell} (list {:name "p", :type :ast-atom}))
      => {:head {:name "p", :type :ast-atom},
          :tail nil,
          :type :ast-cell}

      (prolog/ast-make-node {:head {:name "p", :type :ast-atom},
                             :tail nil,
                             :type :ast-cell} (list {:name "q", :type :ast-atom}
                                                    {:name "r", :type :ast-atom}))
      => {:head {:name "p", :type :ast-atom},
          :tail {:head {:name "q", :type :ast-atom},
                 :tail {:head {:name "r", :type :ast-atom},
                        :tail nil,
                        :type :ast-cell},
                 :type :ast-cell},
          :type :ast-cell}

      (prolog/ast-make-node {:head {:name "p", :type :ast-atom},
                             :tail nil,
                             :type :ast-functor} (list {:name "q", :type :ast-atom}
                                                       {:name "r", :type :ast-atom}))
      => {:head {:name "p", :type :ast-atom},
          :tail {:head {:name "q", :type :ast-atom},
                 :tail {:head {:name "r", :type :ast-atom},
                        :tail nil,
                        :type :ast-cell},
                 :type :ast-cell},
          :type :ast-functor}

      (prolog/ast-make-node {:head {:name "p", :type :ast-atom},
                             :tail {:head {:name "s", :type :ast-atom},
                                    :tail nil,
                                    :type :ast-cell},
                             :type :ast-functor} (list {:name "q", :type :ast-atom}
                                                       {:name "r", :type :ast-atom}))
      => {:head {:name "p", :type :ast-atom},
          :tail {:head {:name "q", :type :ast-atom},
                 :tail {:head {:name "r", :type :ast-atom},
                        :tail {:head {:name "s", :type :ast-atom},
                               :tail nil,
                               :type :ast-cell},
                        :type :ast-cell},
                 :type :ast-cell},
          :type :ast-functor})