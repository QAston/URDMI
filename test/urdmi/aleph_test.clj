(ns urdmi.aleph-test
  (:use midje.sweet
        urdmi.prolog-test)
  (:require [urdmi.plugin.aleph.core :as aleph]
            [urdmi.prolog :as prolog]))

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

            rel-true  (parse-string parser-context
            "pracownik(45,robotnik,1,null,02,1).
pracownik(49,wiceprezes,3,2,00,1).
pracownik(50,prezes,4,2,00,1).")
            rel-out [rel-true rel-false]
            result (aleph/split-by-relation-arg rel-in 6)
            ]
        (first result)  => (just (set (first rel-out)))
        (second result)  => (just (set (second rel-out)))))

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
            (let [result (run-learning  app)]
              (:exit result) => 0
              (string/blank? (:out result)) => false))
          (finally
            (fs/delete-dir workdir-dir)
            (core/move-file backup-working-dir workdir-dir)
            ))))