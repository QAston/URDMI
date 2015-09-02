(ns urdmi.app-test
  (:use midje.sweet)
  (:use urdmi.prolog-test)
  (:use urdmi.app)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [clojure.java.io :as io]
            [urdmi.prolog :as prolog]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [urdmi.app :as app]))

(fact "load-relations loads base relation data from disk"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            loaded (app/load-relations base)
            dzial-6-rel (parse-string (prolog/parser-context nil) "dzial(1,produkcja,produkcyjna,1,null,lapy).
dzial(2,sprzedaz,lipowa,1,1,bialystok).
dzial(3,kontrolajakosci,produkcyjna,1,1,lapy).
dzial(4,marketing,lipowa,1,2,bialystok).
dzial(5,ksiegowosc,lipowa,1,3,bialystok).
dzial(6,informatyka,lipowa,1,4,bialystok).
dzial(7,reklamacja,lipowa,1,5,bialystok).
dzial(8,informatyka,produkcyjna,1,1,lapy).
")
            rels (core/get-relations loaded)
            relkeys (map first (get-in loaded (core/dir-keys core/relations-keyname :dir)))]
        (map :name rels) => (just #{"towar_6.pl" "produkcja_5.pl" "pracownik_7.pl" "pracownikpersonalia_8.pl" "klient_9.pl" "zamowienieszczegoly_4.pl" "pracownikprodukcja_6.pl" "zamowienie_5.pl" "dzial_6.pl"})
        (map :rel rels) => (just #{["dzial" 6] ["klient" 9] ["pracownik" 7] ["pracownikpersonalia" 8] ["pracownikprodukcja" 6] ["produkcja" 5] ["towar" 6] ["zamowienie" 5] ["zamowienieszczegoly" 4]})
        (:ast (core/get-relation-data loaded ["dzial" 6])) => dzial-6-rel
        relkeys => (just #{"towar_6.pl" "produkcja_5.pl" "pracownik_7.pl" "pracownikpersonalia_8.pl" "klient_9.pl" "zamowienieszczegoly_4.pl" "pracownikprodukcja_6.pl" "zamowienie_5.pl" "dzial_6.pl"})))


(fact "load working dir loads base working dir data from disk"
      (let [base (app/load-working-dir (core/base-project (fs/file "dev-resources/projects/ace_tilde/")))
            tildedir (get-in base (core/dir-keys core/workdir-keyname "tilde"))
            outfile (get-in base (core/dir-keys core/workdir-keyname "tilde" "pracownik.out"))]
        (:name tildedir) => "tilde"
        (:name outfile) => "pracownik.out"
        (map first (:dir tildedir)) => (just #{"pracownik.out" "pracownik.progress" "pracownik.ptree"})))

(fact "file-model-zipper allows moving among files"
      (let [base (core/base-project (fs/file "dev-resources/projects/ace_tilde/"))
            workdir (core/workdir-keyname (:dir (app/load-working-dir base)))]
        (zip/node (zip/down (core/file-model-zipper workdir))) => truthy
        (get (:dir (zip/root (zip/append-child (core/file-model-zipper workdir) {:name "tyry"}))) "tyry") => {:name "tyry"}
        ))

(fact "load additions loads additions data from disk"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            additions (get-in (app/load-additions base) (core/dir-keys core/additions-keyname :dir))]
        (map first additions) => (just #{"pracownik.b"})))

(fact "load output"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            additions (get-in (app/load-output base) (core/dir-keys core/output-keyname :dir))]
        (map first additions) => (just #{"result.edn"})))

(fact "init-app loads plugins"
      (let [app (init-app)]
        (map first (:plugins app)) => (just #{:ace :aleph})))

(fact "load settings"
      (let [app (app/load-settings (assoc (init-app) :project (core/base-project (fs/file "dev-resources/projects/aleph_default/"))))
            settings (get-in (:project app) (core/dir-keys core/settings-keyname :dir))]
        (map first settings) => (just #{"project.edn" "aleph.edn"})
        (get-in settings ["project.edn" :data]) => {:working-dir (io/file "working_dir") :active-plugin :aleph}
        (get-in settings ["aleph.edn" :data]) => {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 7}
        (core/get-settings-data (:project app) "aleph.edn") => {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 7}
        (extends? core/Plugin (class (get-in app [:project :plugin]))) => truthy))

(fact "load project populates project fields"
      (let [proj (:project (app/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/")))]
        (< 0 (count (get-in proj (core/dir-keys core/relations-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/additions-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/workdir-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/output-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/settings-keyname :dir)))) => truthy))

(fact "build project generates expected working_dir output for aleph"
      (let [workdir-dir (fs/file "dev-resources/projects/aleph_default/working_dir")
            backup-dir (fs/file "dev-resources/projects/aleph_default/working_dir_orig")]
        (try
          (core/move-file workdir-dir backup-dir)
          (fs/mkdir workdir-dir)
          (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/"))
                parser-context (prolog/aleph-parser-context)]
            (build-working-dir (:project app))
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

(fact "build project generates expected working_dir output for aleph"
      (let [workdir-dir (fs/file "dev-resources/projects/aleph_default/working_dir")
            backup-working-dir (fs/file "dev-resources/projects/aleph_default/working_dir_orig")]
        (try
          (core/move-file workdir-dir backup-working-dir)
          (fs/mkdir workdir-dir)
          (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/"))]
            (build-working-dir (:project app))
            (let [result (run-learning (:project app))]
              (:exit result) => 0
              (string/blank? (:out result)) => false))
          (finally
            (fs/delete-dir workdir-dir)
            (core/move-file backup-working-dir workdir-dir)
            ))))

(fact "build project generates expected working_dir output for ace"
      (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/ace_tilde/"))
            parser-context (prolog/ace-parser-context)
            workdir-dir (core/get-working-dir (:project app))]
        (build-working-dir (:project app))
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
        (build-working-dir (:project app))
        (let [result (run-learning (:project app))]
          (:exit result) => 0
          (string/blank? (:out result)) => false)))

(future-fact "provide [apply] button for settings, settings until 'applied' are stored locally on screen")