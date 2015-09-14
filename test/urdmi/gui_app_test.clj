(ns urdmi.gui-app-test
  (:use midje.sweet)
  (:use urdmi.prolog-test)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [urdmi.gui-app :as gui-app]
            [urdmi.prolog :as prolog]
            [urdmi.app :as app]))

(fact "generate-menu-entries works on example data"
      (let [proj (:project (app/load-project (app/init-app) (fs/file "dev-resources/projects/aleph_default/")))]
        (gui-app/generate-menu-viewmodel proj) =>
        (just #{
          {:name "Project", :path []}
          {:name "Relations", :path [:relations]}
          {:name "towar_6.pl", :path [:relations "towar_6.pl"]}
          {:name "pracownikprodukcja_6.pl",
           :path [:relations "pracownikprodukcja_6.pl"]}
          {:name "produkcja_5.pl", :path [:relations "produkcja_5.pl"]}
          {:name "pracownik_7.pl", :path [:relations "pracownik_7.pl"]}
          {:name "pracownikpersonalia_8.pl",
           :path [:relations "pracownikpersonalia_8.pl"]}
          {:name "klient_9.pl", :path [:relations "klient_9.pl"]}
          {:name "zamowienieszczegoly_4.pl",
           :path [:relations "zamowienieszczegoly_4.pl"]}
          {:name "zamowienie_5.pl", :path [:relations "zamowienie_5.pl"]}
          {:name "dzial_6.pl", :path [:relations "dzial_6.pl"]}
          {:name "Working dir", :path [:working-dir]}
          {:name "pracownik.b", :path [:working-dir "pracownik.b"]}
          {:name "pracownik.f", :path [:working-dir "pracownik.f"]}
          {:name "pracownik.n", :path [:working-dir "pracownik.n"]}
          {:name "Output", :path [:output]}
          {:name "result.edn", :path [:output "result.edn"]}
          {:name "Additions", :path [:additions]}
          {:name "pracownik.b", :path [:additions "pracownik.b"]}
          {:name "Settings", :path [:settings]}
          {:name "project.edn", :path [:settings "project.edn"]}})))

(fact gui-app/relations-model-to-viewmodel
      (fact "converts valid relations"
            (let [proj (:project (app/load-project (app/init-app) (fs/file "dev-resources/projects/aleph_default/")))]
              (gui-app/relations-model-to-viewmodel (core/get-relation-data proj ["dzial" 6])) =>
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
            (gui-app/relations-model-to-viewmodel {:rel ["t" 2]
                                                   :ast (parse-string
                                                          (prolog/parser-context []) "t(urdmi_edit(''),urdmi_edit('1atom')).t(1,Tyry).")}) =>
            {:name  "t"
             :arity 2
             :items [["" "1atom"]
                     ["1" "Tyry"]]}))

(facts gui-app/relations-viewmodel-to-model
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
               (gui-app/relations-viewmodel-to-model (prolog/parser-context []) viewmodel) =>
               {:rel ["dzial" 6]
                :name "dzial_6.pl"
                :ast ast}))
       (fact "invalid entries are wrapped in urdmi_edit()"
             (let [viewmodel {:name  "t"
                              :arity 2
                              :items [["" "1atom"]
                                      ["1" "Tyry"]]}
                   ast (parse-string (prolog/parser-context nil) "t(urdmi_edit(''),urdmi_edit('1atom')).t(1,Tyry).")]
               (gui-app/relations-viewmodel-to-model (prolog/parser-context []) viewmodel) =>
               {:rel ["t" 2]
                :name "t_2.pl"
                :ast ast})))