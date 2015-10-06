(ns urdmi.gui.relation-test
  (:use midje.sweet)
  (:use urdmi.prolog-test)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [urdmi.gui.relation :as relation]
            [urdmi.prolog :as prolog]
            [urdmi.app :as app]))

(fact relation/relations-model-to-viewmodel
      (fact "converts valid relations"
            (let [proj (:project (app/load-project (app/init-app) (fs/file "dev-resources/projects/aleph_default/")))]
              (relation/relations-model-to-viewmodel (prolog/parser-context [])
                                                    (core/get-relation proj ["dzial" 6])) =>
              {:name  "dzial"
               :arity 6
               :items [["1" "produkcja" "produkcyjna" "1" "null" "lapy"]
                       ["2" "sprzedaz" "lipowa" "1" "1" "bialystok"]
                       ["3" "kontrolajakosci" "produkcyjna" "1" "1" "lapy"]
                       ["4" "marketing" "lipowa" "1" "2" "bialystok"]
                       ["5" "ksiegowosc" "lipowa" "1" "3" "bialystok"]
                       ["6" "informatyka" "lipowa" "1" "4" "bialystok"]
                       ["7" "reklamacja" "lipowa" "1" "5" "bialystok"]
                       ["8" "informatyka" "produkcyjna" "1" "1" "lapy"]]}))
      (fact "invalid entries are wrapped in urdmi_edit()"
            (relation/relations-model-to-viewmodel (prolog/parser-context [])
                                                  {:rel ["t" 2]
                                                   :ast (parse-string
                                                          (prolog/parser-context []) "t(urdmi_edit(''),urdmi_edit('1atom')).t(1,Tyry).")}) =>
            {:name  "t"
             :arity 2
             :items [["" "1atom"]
                     ["1" "Tyry"]]}))

(facts relation/relations-viewmodel-to-model
       (fact "converts valid relations"
             (let [viewmodel {:name  "dzial"
                              :arity 6
                              :items [["1" "produkcja" "produkcyjna" "1" "null" "lapy"]
                                      ["2" "sprzedaz" "lipowa" "1" "1" "bialystok"]
                                      ["3" "kontrolajakosci" "produkcyjna" "1" "1" "lapy"]
                                      ["4" "marketing" "lipowa" "1" "2" "bialystok"]
                                      ["5" "ksiegowosc" "lipowa" "1" "3" "bialystok"]
                                      ["6" "informatyka" "lipowa" "1" "4" "bialystok"]
                                      ["7" "reklamacja" "lipowa" "1" "5" "bialystok"]
                                      ["8" "informatyka" "produkcyjna" "1" "1" "lapy"]]}
                   ast (parse-string (prolog/parser-context nil) "dzial(1,produkcja,produkcyjna,1,null,lapy).
dzial(2,sprzedaz,lipowa,1,1,bialystok).
dzial(3,kontrolajakosci,produkcyjna,1,1,lapy).
dzial(4,marketing,lipowa,1,2,bialystok).
dzial(5,ksiegowosc,lipowa,1,3,bialystok).
dzial(6,informatyka,lipowa,1,4,bialystok).
dzial(7,reklamacja,lipowa,1,5,bialystok).
dzial(8,informatyka,produkcyjna,1,1,lapy).
")
                   ]
               (relation/relations-viewmodel-to-model (prolog/parser-context []) viewmodel) =>
               {:rel  ["dzial" 6]
                :name "dzial_6.pl"
                :ast  ast}))
       (fact "invalid entries are wrapped in urdmi_edit()"
             (let [viewmodel {:name  "t"
                              :arity 2
                              :items [["" "1atom"]
                                      ["1" "Tyry"]]}
                   ast (parse-string (prolog/parser-context nil) "t(urdmi_edit(''),urdmi_edit('1atom')).t(1,Tyry).")]
               (relation/relations-viewmodel-to-model (prolog/parser-context []) viewmodel) =>
               {:rel  ["t" 2]
                :name "t_2.pl"
                :ast  ast})))