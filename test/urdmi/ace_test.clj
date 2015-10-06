(ns urdmi.ace-test)

(fact "build project generates expected working_dir output for ace"
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

(fact "build project generates expected working_dir output for ace"
      (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/ace_tilde/"))]
        (build-working-dir app)
        (let [result (run-learning app)]
          (:exit result) => 0
          (string/blank? (:out result)) => false)))
