(ns urdmi.prolog-test
  (:use midje.sweet)
  (:import java.io.StringReader)
  (:require
    [urdmi.prolog :as prolog]
    [clojure.java.io :as io]))

(fact "prolog expr seq returns a seq of ast nodes"
      (let [empty (prolog/prolog-sentence-seq (StringReader. ""))
            single-sentence (prolog/prolog-sentence-seq (StringReader. "hello :- world."))
            two-sentences (prolog/prolog-sentence-seq (StringReader. "hello :- world. parent(a):- mother."))
            ;file (prolog/prolog-sentence-seq (io/make-reader (io/resource "aleph/gfather.b") {}))
            ]
        (count single-sentence) => 1
        (count two-sentences) => 2
        (count empty) => 0
        (type (first two-sentences)) => com.igormaznitsa.prologparser.terms.PrologStructure
        ;(count file) => 26 ; TODO: this fails, get a better parser
        )
      )

