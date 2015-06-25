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
                                                            :type :ast-functor})))

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
      (let [parser-context (prolog/ace-parser-context)
            test-sentences ["hello :- world."
                            "p(X)."
                            "p(+X)."
                            "q(+-Y)."
                            "q(-Y)."
                            "q(\\Y)."
                            "q(+\\Y)."
                            "q(-\\Y)."
                            "q(+-\\Y)."
                            ]]
        (doseq [sentence test-sentences]
          (str/join (filter #(not= % \space)
                            (prolog/pretty-print-sentences
                              parser-context
                              (parse-string parser-context sentence)))) => (str/join (filter #(not= % \space) (butlast sentence))))))