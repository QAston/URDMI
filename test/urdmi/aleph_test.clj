(ns urdmi.aleph-test
  (:use midje.sweet
        urdmi.prolog-test
        urdmi.test-util)
  (:require [urdmi.plugin.aleph.core :as aleph]
            [urdmi.prolog :as prolog]
            [urdmi.core :as core]
            [urdmi.app :as app]
            [me.raynes.fs :as fs]
            [clojure.string :as string]))

(use 'clojure.pprint)
(fact 'aleph/split-by-relation-arg
      (let [parser-context (prolog/aleph-parser-context)
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

            rel-true (parse-string parser-context
                                   "pracownik(45,robotnik,1,null,02,1).
                       pracownik(49,wiceprezes,3,2,00,1).
                       pracownik(50,prezes,4,2,00,1).")
            result (aleph/split-by-relation-arg rel-in 6 parser-context)
            ]
        (result "1") => (just (set rel-true))
        (result "0") => (just (set rel-false))))


(def adv-ex-data [{:relation      ["pracownik" 7]
                   :relation-term 6
                   :value-type    :positive
                   :value         "1"}
                  {:relation      ["pracownik" 7]
                   :relation-term 6
                   :value-type    :negative
                   :value         "0"}])

(facts 'aleph/get-advanced-example-data-settings
       (aleph/get-advanced-example-data-settings ["pracownik" 7] 6 "1" "0") => adv-ex-data)

(facts 'aleph/get-learning-examples-settings-project
       (let [project (make-dummy-project [[:settings aleph/datamining-name] (core/file-item {:example {:type :simple, :relation ["pracownik" 7], :term 6, :true-val "1", :false-val "0", :advanced-list []}, :background nil, :program "induce"})])]
         (aleph/get-learning-examples-settings-project project) => adv-ex-data)
       (let [project (make-dummy-project [[:settings aleph/datamining-name] (core/file-item {:example {:type :advanced, :relation nil, :term 6, :true-val "1", :false-val "0", :advanced-list adv-ex-data}, :background nil, :program "induce"})])]
         (aleph/get-learning-examples-settings-project project) => adv-ex-data))

(defn load-default-app []
  (app/load-project (app/init-app) (fs/file "dev-resources/projects/aleph_default/")))

(facts 'aleph/get-background-relations
       (fact "all but examples"
             (let [p (modify-dummy-project (:project (load-default-app))
                                           [[:settings aleph/datamining-name] (core/file-item {:example {:type :simple, :relation ["pracownik" 7], :term 6, :true-val "1", :false-val "0", :advanced-list []}, :background {:type :all-but-example, :relation-list nil}, :program "induce"})])]
               (aleph/get-background-relations p) => (just ["dzial" 6] ["klient" 9] ["pracownikpersonalia" 8] ["pracownikprodukcja" 6] ["produkcja" 5] ["towar" 6] ["zamowienie" 5] ["zamowienieszczegoly" 4])
               ))
       (fact "all"
             (let [p (modify-dummy-project (:project (load-default-app))
                                           [[:settings aleph/datamining-name] (core/file-item {:example {:type :simple, :relation ["pracownik" 7], :term 6, :true-val "1", :false-val "0", :advanced-list []}, :background {:type :all, :relation-list nil}, :program "induce"})])]
               (aleph/get-background-relations p) => (just ["dzial" 6] ["klient" 9] ["pracownik" 7] ["pracownikpersonalia" 8] ["pracownikprodukcja" 6] ["produkcja" 5] ["towar" 6] ["zamowienie" 5] ["zamowienieszczegoly" 4])
               ))
       (fact "selected"
             (let [p (modify-dummy-project (:project (load-default-app))
                                           [[:settings aleph/datamining-name] (core/file-item {:example {:type :simple, :relation ["pracownik" 7], :term 6, :true-val "1", :false-val "0", :advanced-list []}, :background {:type :selected, :relation-list [["dzial" 6] ["klient" 9] ["pracownik" 7]]}, :program "induce"})])]
               (aleph/get-background-relations p) => (just ["dzial" 6] ["klient" 9] ["pracownik" 7])
               )))

(facts 'aleph/get-modeh-settings
       (let [test-hypothesis-list {["dzial" 6] [["pracownikpersonalia" 8] ["produkcja" 5] ["dzial" 6] ["towar" 6] ["klient" 9] ["zamowienie" 5] ["not" 1] ["zamowienieszczegoly" 4]]}

             p (modify-dummy-project (:project (load-default-app))
                                     [[:settings aleph/datamining-name] (core/file-item {:example {:type :simple, :relation ["pracownik" 7], :term 6, :true-val "1", :false-val "0", :advanced-list []}, :background {:type :all-but-example, :relation-list nil}, :program "induce"})]
                                     [[:settings aleph/hypothesis-name] (core/file-item {:clause {:clause-list [{:relation ["pracownikpersonalia" 8], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "miejscowosc"}]} {:relation ["zamowienieszczegoly" 4], :determinacy "1", :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idtowaru"} {:type "+", :value "ilosc"} {:type "+", :value "upust"}]} {:relation ["produkcja" 5], :determinacy "1", :terms [{:type "+", :value "idtowaru"} {:type "+", :value "idpracownika"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"}]} {:relation ["dzial" 6], :determinacy "1", :terms [{:type "+", :value "iddzialu"} {:type "+", :value "nazwa"} {:type "+", :value "ulica"} {:type "+", :value "nrdomudzial"} {:type "+", :value "nrlokaludzial"} {:type "+", :value "miejscowosc"}]} {:relation ["towar" 6], :determinacy "1", :terms [{:type "+", :value "idtowaru"} {:type "+", :value "opis"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"} {:type "+", :value "typename"}]} {:relation ["klient" 9], :determinacy "1", :terms [{:type "+", :value "idklienta"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "kod"} {:type "+", :value "miejscowosc"}]} {:relation ["zamowienie" 5], :determinacy "1", :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idklienta"} {:type "+", :value "idpracownika"} {:type "+", :value "data"} {:type "+", :value "platnosc"}]} {:relation ["pracownik" 7], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "+", :value "stanowisko"} {:type "+", :value "pensja"} {:type "+", :value "premia"} {:type "+", :value "rokzatrudnienia"} {:type "+", :value "iddzialu"} {:type "+", :value "grupa"}]}]}, :hypothesis {:autogenerate-hypothesis false, :hypothesis-list test-hypothesis-list}})])]
         (aleph/get-modeh-settings p) => [{:determinacy "1", :relation ["pracownikpersonalia" 8], :terms [{:type "+", :value "idpracownika"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "miejscowosc"}]} {:determinacy "1", :relation ["zamowienieszczegoly" 4], :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idtowaru"} {:type "+", :value "ilosc"} {:type "+", :value "upust"}]} {:determinacy "1", :relation ["produkcja" 5], :terms [{:type "+", :value "idtowaru"} {:type "+", :value "idpracownika"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"}]} {:determinacy "1", :relation ["dzial" 6], :terms [{:type "+", :value "iddzialu"} {:type "+", :value "nazwa"} {:type "+", :value "ulica"} {:type "+", :value "nrdomudzial"} {:type "+", :value "nrlokaludzial"} {:type "+", :value "miejscowosc"}]} {:determinacy "1", :relation ["towar" 6], :terms [{:type "+", :value "idtowaru"} {:type "+", :value "opis"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"} {:type "+", :value "typename"}]} {:determinacy "1", :relation ["klient" 9], :terms [{:type "+", :value "idklienta"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "kod"} {:type "+", :value "miejscowosc"}]} {:determinacy "1", :relation ["zamowienie" 5], :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idklienta"} {:type "+", :value "idpracownika"} {:type "+", :value "data"} {:type "+", :value "platnosc"}]} {:determinacy "1", :relation ["pracownik" 6], :terms [{:type "+", :value "idpracownika"} {:type "+", :value "stanowisko"} {:type "+", :value "pensja"} {:type "+", :value "premia"} {:type "+", :value "rokzatrudnienia"} {:type "+", :value "iddzialu"}]}]
         ))

(facts 'aleph/generate-hypothesis-settings-from-learning-example-settings
       (let [p (modify-dummy-project (:project (load-default-app))
                                     [[:settings aleph/datamining-name] (core/file-item {:example {:type :simple, :relation ["pracownik" 7], :term 6, :true-val "1", :false-val "0", :advanced-list []}, :background {:type :all-but-example, :relation-list nil}, :program "induce"})]
                                     [[:settings aleph/hypothesis-name] (core/file-item {:clause {:clause-list [{:relation ["pracownikpersonalia" 8], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "miejscowosc"}]} {:relation ["zamowienieszczegoly" 4], :determinacy "1", :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idtowaru"} {:type "+", :value "ilosc"} {:type "+", :value "upust"}]} {:relation ["produkcja" 5], :determinacy "1", :terms [{:type "+", :value "idtowaru"} {:type "+", :value "idpracownika"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"}]} {:relation ["dzial" 6], :determinacy "1", :terms [{:type "+", :value "iddzialu"} {:type "+", :value "nazwa"} {:type "+", :value "ulica"} {:type "+", :value "nrdomudzial"} {:type "+", :value "nrlokaludzial"} {:type "+", :value "miejscowosc"}]} {:relation ["towar" 6], :determinacy "1", :terms [{:type "+", :value "idtowaru"} {:type "+", :value "opis"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"} {:type "+", :value "typename"}]} {:relation ["klient" 9], :determinacy "1", :terms [{:type "+", :value "idklienta"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "kod"} {:type "+", :value "miejscowosc"}]} {:relation ["zamowienie" 5], :determinacy "1", :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idklienta"} {:type "+", :value "idpracownika"} {:type "+", :value "data"} {:type "+", :value "platnosc"}]} {:relation ["pracownik" 7], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "+", :value "stanowisko"} {:type "+", :value "pensja"} {:type "+", :value "premia"} {:type "+", :value "rokzatrudnienia"} {:type "+", :value "iddzialu"} {:type "+", :value "grupa"}]}]}, :hypothesis {:autogenerate-hypothesis true, :hypothesis-list {["dzial" 6] [["pracownikpersonalia" 8] ["produkcja" 5] ["dzial" 6] ["towar" 6] ["klient" 9] ["zamowienie" 5] ["not" 1] ["zamowienieszczegoly" 4]]}}})])]
         (aleph/generate-hypothesis-settings-from-learning-example-settings p) => {["pracownik" 6] [["dzial" 6] ["klient" 9] ["pracownikpersonalia" 8] ["produkcja" 5] ["towar" 6] ["zamowienie" 5] ["zamowienieszczegoly" 4]]}
         ))

(facts 'aleph/get-hypothesis-list
       (let [test-hypothesis-list {["dzial" 6] [["pracownikpersonalia" 8] ["produkcja" 5] ["dzial" 6] ["towar" 6] ["klient" 9] ["zamowienie" 5] ["not" 1] ["zamowienieszczegoly" 4]]}

             p (modify-dummy-project (:project (load-default-app))
                                     [[:settings aleph/datamining-name] (core/file-item {:example {:type :simple, :relation ["pracownik" 7], :term 6, :true-val "1", :false-val "0", :advanced-list []}, :background {:type :all-but-example, :relation-list nil}, :program "induce"})]
                                     [[:settings aleph/hypothesis-name] (core/file-item {:clause {:clause-list [{:relation ["pracownikpersonalia" 8], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "miejscowosc"}]} {:relation ["zamowienieszczegoly" 4], :determinacy "1", :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idtowaru"} {:type "+", :value "ilosc"} {:type "+", :value "upust"}]} {:relation ["produkcja" 5], :determinacy "1", :terms [{:type "+", :value "idtowaru"} {:type "+", :value "idpracownika"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"}]} {:relation ["dzial" 6], :determinacy "1", :terms [{:type "+", :value "iddzialu"} {:type "+", :value "nazwa"} {:type "+", :value "ulica"} {:type "+", :value "nrdomudzial"} {:type "+", :value "nrlokaludzial"} {:type "+", :value "miejscowosc"}]} {:relation ["towar" 6], :determinacy "1", :terms [{:type "+", :value "idtowaru"} {:type "+", :value "opis"} {:type "+", :value "poczatek"} {:type "+", :value "koniec"} {:type "+", :value "budzet"} {:type "+", :value "typename"}]} {:relation ["klient" 9], :determinacy "1", :terms [{:type "+", :value "idklienta"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "kod"} {:type "+", :value "miejscowosc"}]} {:relation ["zamowienie" 5], :determinacy "1", :terms [{:type "+", :value "idzamowienia"} {:type "+", :value "idklienta"} {:type "+", :value "idpracownika"} {:type "+", :value "data"} {:type "+", :value "platnosc"}]} {:relation ["pracownik" 7], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "+", :value "stanowisko"} {:type "+", :value "pensja"} {:type "+", :value "premia"} {:type "+", :value "rokzatrudnienia"} {:type "+", :value "iddzialu"} {:type "+", :value "grupa"}]}]}, :hypothesis {:autogenerate-hypothesis false, :hypothesis-list test-hypothesis-list}})])]
         (aleph/get-hypothesis-list p) => test-hypothesis-list
         ))

(facts 'aleph/format-mode
       (let [mode {:relation ["pracownikpersonalia" 8], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "nrlokalu"} {:type "+", :value "miejscowosc"}]}]
         (string/trim (aleph/format-mode mode)) => ":-mode(1,pracownikpersonalia(+idpracownika,+imie,+nazwisko,+nip,+ulica,+nrdomu,+nrlokalu,+miejscowosc))."))

(facts 'aleph/format-determination
       (string/trim (aleph/format-determination ["pracownikpersonalia" 8] ["produkcja" 5])) => ":-determination(pracownikpersonalia/8,produkcja/5).")

(facts 'aleph/convert-mode-spec-for-example-modeh
       (aleph/convert-mode-spec-for-example-modeh {:relation ["pracownikpersonalia" 8], :determinacy "1", :terms [{:type "+", :value "idpracownika"} {:type "-", :value "imie"} {:type "-", :value "nazwisko"} {:type "-", :value "nip"} {:type "-", :value "ulica"} {:type "-", :value "nrdomu"} {:type "-", :value "nrlokalu"} {:type "-", :value "miejscowosc"}]} 6) => {:determinacy "1", :relation ["pracownikpersonalia" 7], :terms [{:type "+", :value "idpracownika"} {:type "+", :value "imie"} {:type "+", :value "nazwisko"} {:type "+", :value "nip"} {:type "+", :value "ulica"} {:type "+", :value "nrdomu"} {:type "+", :value "miejscowosc"}]})

#_(fact "build project generates expected working_dir output for aleph"
        (let [workdir-dir (fs/file "dev-resources/projects/aleph_default/working_dir")
              backup-dir (fs/file "dev-resources/projects/aleph_default/working_dir_orig")]
          (try
            (core/move-file workdir-dir backup-dir)
            (fs/mkdir workdir-dir)
            (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/"))
                  parser-context (prolog/aleph-parser-context)]
              (build-working-dir app)
              (fact "pracownik.f"
                    (with-open [rdr (io/reader (io/file workdir-dir "pracownik.f"))]
                      (let [sentences (doall (prolog/prolog-sentence-seq parser-context rdr))]
                        (count sentences) => 17
                        (distinct (map (fn [sentence]
                                         (:name (first (:children sentence)))) sentences)) => (list "pracownik"))))
              (fact "pracownik.n"
                    (with-open [rdr (io/reader (io/file workdir-dir "pracownik.n"))]
                      (let [sentences (doall (prolog/prolog-sentence-seq parser-context rdr))]
                        (count sentences) => 17
                        (distinct (map (fn [sentence]
                                         (:name (first (:children sentence)))) sentences)) => (list "pracownik"))))
              (fact "pracownik.b"
                    (with-open [rdr (io/reader (io/file workdir-dir "pracownik.b"))]
                      (let [sentences (doall (prolog/prolog-sentence-seq parser-context rdr))]
                        (count sentences) => 1415
                        (distinct (map (fn [sentence]
                                         (:name (first (:children sentence)))) sentences)) => (just #{"dzial" "klient" "pracownikpersonalia" "pracownikprodukcja"
                                                                                                      "produkcja" "towar" "zamowienie"
                                                                                                      "zamowienieszczegoly" ":-"})))))
            (finally
              (fs/delete-dir workdir-dir)
              (core/move-file backup-dir workdir-dir)
              ))))

#_(fact "build project generates expected working_dir output for aleph"
        (let [workdir-dir (fs/file "dev-resources/projects/aleph_default/working_dir")
              backup-working-dir (fs/file "dev-resources/projects/aleph_default/working_dir_orig")]
          (try
            (core/move-file workdir-dir backup-working-dir)
            (fs/mkdir workdir-dir)
            (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/"))]
              (build-working-dir app)
              (let [result (run-learning app)]
                (:exit result) => 0
                (string/blank? (:out result)) => false))
            (finally
              (fs/delete-dir workdir-dir)
              (core/move-file backup-working-dir workdir-dir)
              ))))