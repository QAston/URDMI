(ns urdmi.relation-test
  (:use midje.sweet)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [urdmi.gui.relation :as relation]))

(fact "generate-menu-entries works on example data"
      (let [proj (core/load-base-project (fs/file "dev-resources/projects/aleph_default/"))]
        (relation/generate-viewmodel (core/get-relation-data proj ["dzial" 6])) =>
        {:name  "dzial"
         :arity 6
         :data  [["1" "produkcja" "produkcyjna" "1" "null" "lapy"]
                 ["2" "sprzedaz" "lipowa" "1" "1" "bialystok"]
                 ["3" "kontrolajakosci" "produkcyjna" "1" "1" "lapy"]
                 ["4" "marketing" "lipowa" "1" "2" "bialystok"]
                 ["5" "ksiegowosc" "lipowa" "1" "3" "bialystok"]
                 ["6" "informatyka" "lipowa" "1" "4" "bialystok"]
                 ["7" "reklamacja" "lipowa" "1" "5" "bialystok"]
                 ["8" "informatyka" "produkcyjna" "1" "1" "lapy"]]}))
