(ns urdmi.gui-test
  (:use midje.sweet)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [urdmi.gui :as gui]))

(fact "generate-menu-entries works on example data"
      (let [proj (core/load-base-project (fs/file "dev-resources/projects/aleph_default/"))]
         (gui/generate-menu-entries proj) =>
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