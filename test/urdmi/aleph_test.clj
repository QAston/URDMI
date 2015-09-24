(ns urdmi.aleph-test
  (:use midje.sweet
        urdmi.prolog-test)
  (:require [urdmi.plugin.aleph.core :as aleph]
            [urdmi.prolog :as prolog]))

(use 'clojure.pprint)
(fact 'aleph/split-by-relation-arg
      (let [parser-context (prolog/ace-parser-context)
            rel-in (parse-string parser-context "pracownik(1,sprzedawca,1,null,08,2,0).
pracownik(45,robotnik,1,null,02,1,1).
pracownik(49,wiceprezes,3,2,00,1,1).
pracownik(50,prezes,4,2,00,1,1).
pracownik(2,sprzedawca,1,null,09,2,0).
pracownik(3,sprzedawca,1,null,09,2,0).")

            rel-false (parse-string parser-context
            "pracownik(1,sprzedawca,1,null,08,2).
pracownik(2,sprzedawca,1,null,09,2).
pracownik(3,sprzedawca,1,null,09,2).")

            rel-true  (parse-string parser-context
            "pracownik(45,robotnik,1,null,02,1).
pracownik(49,wiceprezes,3,2,00,1).
pracownik(50,prezes,4,2,00,1).")
            rel-out [rel-true rel-false]
            result (aleph/split-by-relation-arg rel-in 7)
            ]
        (first result)  => (just (set (first rel-out)))
        (second result)  => (just (set (second rel-out)))))