(ns urdmi.gui-app
  (:require [clojure.core.async :refer [chan go <! >!]]
            [urdmi.core :as core]
            [clojure.zip :as zip]
            [urdmi.gui.main :as main-gui]
            [urdmi.prolog :as prolog]
            [fx-clj.core :as fx])
  (:import (urdmi.core Project)
           (java.io StringWriter)))

(defn- rel-ast-to-table [rel-asts]
  (let [op-manager (:op-manager (prolog/parser-context []))]
    (->> rel-asts
         (mapv (fn [ast]
                 (->> ast
                      :children
                      rest
                      (mapv (fn [ast]
                              (let [writer (StringWriter.)]
                                (prolog/pretty-print ast op-manager writer)
                                (.toString writer))))))))))

(defn generate-relations-viewmodel [rel]
  (let [rel-asts (:ast rel)
        [rel-name rel-arity] (:rel rel)]
    {:name  rel-name
     :arity rel-arity
     :items (rel-ast-to-table rel-asts)}
    ))

(defn- file-names-recursively [zipiter proj-key]
  (let [path (into [proj-key] (conj (mapv :name (rest (zip/path zipiter))) (:name (zip/node zipiter))))
        name (:name (zip/node zipiter))]
    (if (zip/branch? zipiter)
      (vector
        (keep identity (cons [:name (:name (zip/node zipiter)) :path path]
                             (if-let [childiter (zip/down zipiter)]
                               (loop [iter childiter
                                      children []
                                      ]
                                 (if-not (nil? iter)
                                   (recur (zip/right iter) (conj children (file-names-recursively iter proj-key)))
                                   children))

                               (list)
                               ))))
      {:name name :path path})))

(defn- get-file-names [^Project p proj-key display-name]
  (vec (cons {:name display-name :path [proj-key]}
             (rest (first (file-names-recursively (core/file-model-zipper (get-in p (core/dir-keys proj-key))) proj-key))))))

(defn generate-menu-viewmodel [^Project p]
  (let [
        relations (get-file-names p core/relations-keyname "Relations")
        working-dir (get-file-names p core/workdir-keyname "Working dir")
        outputs (get-file-names p core/output-keyname "Output")
        additions (get-file-names p core/output-keyname "Additions")
        settings (get-file-names p core/output-keyname "Settings")
        ]
    [{:name "Project" :path []} relations working-dir outputs additions settings]))

(defn test-fn[]
  (let [ui-events (chan)
        main-screen (main-gui/make-main-screen ui-events)
        files-view-model [{:name "Project", :path []}
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
                           {:name "result.edn", :path [:output "result.edn"]}]]]
    (main-gui/set-file-menu-data! main-screen files-view-model)
    (main-gui/get-widget main-screen)))

(fx/sandbox #'test-fn)