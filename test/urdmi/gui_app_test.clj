(ns urdmi.gui-app-test
  (:use midje.sweet)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [urdmi.gui-app :as gui-app]))

(fact "generate-menu-entries works on example data"
      (let [proj (core/load-base-project (fs/file "dev-resources/projects/aleph_default/"))]
        (gui-app/generate-menu-viewmodel proj) =>
        [{:name "Project", :path []}
         [{:name "Relations", :path [:relations]}
          {:name "towar_6.pl", :path [:relations "towar_6.pl"]}
          {:name "produkcja_5.pl", :path [:relations "produkcja_5.pl"]}
          {:name "pracownik_7.pl", :path [:relations "pracownik_7.pl"]}
          {:name "pracownikpersonalia_8.pl",
           :path [:relations "pracownikpersonalia_8.pl"]}
          {:name "klient_9.pl", :path [:relations "klient_9.pl"]}
          {:name "zamowienieszczegoly_4.pl",
           :path [:relations "zamowienieszczegoly_4.pl"]}
          {:name "pracownikprodukcja_7.pl",
           :path [:relations "pracownikprodukcja_7.pl"]}
          {:name "zamowienie_5.pl", :path [:relations "zamowienie_5.pl"]}
          {:name "dzial_6.pl", :path [:relations "dzial_6.pl"]}]
         [{:name "Working dir", :path [:working-dir]}
          {:name "pracownik.b", :path [:working-dir "pracownik.b"]}
          {:name "pracownik.f", :path [:working-dir "pracownik.f"]}
          {:name "pracownik.n", :path [:working-dir "pracownik.n"]}]
         [{:name "Output", :path [:output]}
          {:name "result.edn", :path [:output "result.edn"]}]
         [{:name "Additions", :path [:output]}
          {:name "result.edn", :path [:output "result.edn"]}]
         [{:name "Settings", :path [:output]}
          {:name "result.edn", :path [:output "result.edn"]}]]))

(fact "generate-menu-entries works on example data"
      (let [proj (core/load-base-project (fs/file "dev-resources/projects/aleph_default/"))]
        (gui-app/generate-relations-viewmodel (core/get-relation-data proj ["dzial" 6])) =>
        {:name  "dzial"
         :arity 6
         :items  [["1" "produkcja" "produkcyjna" "1" "null" "lapy"]
                 ["2" "sprzedaz" "lipowa" "1" "1" "bialystok"]
                 ["3" "kontrolajakosci" "produkcyjna" "1" "1" "lapy"]
                 ["4" "marketing" "lipowa" "1" "2" "bialystok"]
                 ["5" "ksiegowosc" "lipowa" "1" "3" "bialystok"]
                 ["6" "informatyka" "lipowa" "1" "4" "bialystok"]
                 ["7" "reklamacja" "lipowa" "1" "5" "bialystok"]
                 ["8" "informatyka" "produkcyjna" "1" "1" "lapy"]]}))