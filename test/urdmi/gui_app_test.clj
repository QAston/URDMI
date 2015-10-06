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
                {:name "Prolog ext", :path [:prolog-ext]}
                {:name "pracownik.b", :path [:prolog-ext "pracownik.b"]}
                {:name "Settings", :path [:settings]}
                {:name "project.edn", :path [:settings "project.edn"]}})))