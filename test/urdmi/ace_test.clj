(ns urdmi.ace-test
  (:use midje.sweet
        urdmi.prolog-test
        clojure.pprint)
  (:require [urdmi.plugin.ace.core :as ace]
            [urdmi.prolog :as prolog]))

(fact 'ace/relations-to-model-format
      (let [parser-context (prolog/ace-parser-context)
            relations-map {["pracownik" 7]
                           (parse-string parser-context "pracownik(1,sprzedawca,1,null,08,2,0).
pracownik(45,robotnik,1,null,02,1,1).
pracownik(49,wiceprezes,3,2,00,1,1).
pracownik(50,prezes,4,2,00,1,1).
pracownik(2,sprzedawca,1,null,09,2,0).
pracownik(3,sprzedawca,1,null,09,2,0).")
                           ["pos" 1]
                           (parse-string parser-context "
pos(45).
pos(49).
pos(50).")
                           ["neg" 1]
                           (parse-string parser-context "
neg(1).
neg(2).
neg(3).")

                           }
            model-relation ["pracownik" 7]

            expected-result (parse-string parser-context "
begin(model(1)).
pracownik(sprzedawca,1,null,08,2,0).
end(model(1)).
begin(model(45)).
pracownik(robotnik,1,null,02,1,1).
pos.
end(model(45)).
begin(model(49)).
pracownik(wiceprezes,3,2,00,1,1).
pos.
end(model(49)).
begin(model(50)).
pracownik(prezes,4,2,00,1,1).
pos.
end(model(50)).
begin(model(2)).
pracownik(sprzedawca,1,null,09,2,0).
end(model(2)).
begin(model(3)).
pracownik(sprzedawca,1,null,09,2,0).
end(model(3)).")

            result (ace/relations-to-model-format relations-map model-relation 0 [[["pos" 1] 0]])
            ]
        (gui-diff-data (nested-sort (into (list) expected-result)) (into (list) (nested-sort result)))
        ;result => expected-result
        ))

#_(fact "build project generates expected working_dir output for ace"
      (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/ace_tilde/"))
            parser-context (prolog/ace-parser-context)
            workdir-dir (core/get-working-dir (:project app))]
        (build-working-dir app)
        (fact "pracownik.s"
              (with-open [rdr (io/reader (io/file workdir-dir "pracownik.s"))]
                (let [sentences (doall (prolog/prolog-sentence-seq parser-context rdr))]
                  (count sentences) => 26
                  (distinct (map (fn [sentence]
                                   (:name (first (:children sentence)))) sentences)) => (list "load" "tilde_mode" "talking" "predict" "typed_language" "type" "rmode"))))
        (fact "pracownik.kb"
              (with-open [rdr (io/reader (io/file workdir-dir "pracownik.kb"))]
                (let [sentences (doall (prolog/prolog-sentence-seq parser-context rdr))]
                  (count sentences) => 34
                  (distinct (map (fn [sentence]
                                   (:name (first (:children sentence)))) sentences)) => (list "pracownik"))))
        (fact "pracownik.bg"
              (with-open [rdr (io/reader (io/file workdir-dir "pracownik.bg"))]
                (let [sentences (doall (prolog/prolog-sentence-seq parser-context rdr))]
                  (count sentences) => 1379
                  (distinct (map (fn [sentence]
                                   (:name (first (:children sentence)))) sentences)) => (just #{"dzial" "klient" "pracownikpersonalia" "pracownikprodukcja"
                                                                                                "produkcja" "towar" "zamowienie"
                                                                                                "zamowienieszczegoly"}))))))

#_(fact "build project generates expected working_dir output for ace"
      (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/ace_tilde/"))]
        (build-working-dir app)
        (let [result (run-learning app)]
          (:exit result) => 0
          (string/blank? (:out result)) => false)))
