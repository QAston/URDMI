(ns urdmi.app-test
  (:use midje.sweet)
  (:use urdmi.app)
  (:use clojure.pprint)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [clojure.java.io :as io]
            [urdmi.prolog :as prolog])
  (:import (urdmi.core App)
           (java.io File)))

(fact "init-app loads plugins"
      (let [app (init-app)]
        (map first (:plugins app)) => (just #{:ace :aleph})))

(fact "load settings"
      (let [app (core/load-settings (assoc (init-app) :project (core/base-project (fs/file "dev-resources/projects/aleph_default/"))))
            settings (get-in (:project app) (core/dir-keys core/settings-keyname :dir))]
        (map first settings) => (just #{"project.edn" "aleph.edn"})
        (get-in settings ["project.edn" :data]) => {:working-dir (io/file "working_dir") :active-plugin :aleph}
        (get-in settings ["aleph.edn" :data]) => {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 7}
        (core/get-settings-data (:project app) "aleph.edn") => {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 7}
        (extends? core/Plugin (class (get-in app [:project :plugin]))) => truthy))

(fact "load project populates project fields"
      (let [proj (:project (core/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/")))]
        (< 0 (count (get-in proj (core/dir-keys core/relations-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/additions-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/workdir-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/output-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/settings-keyname :dir)))) => truthy))

(fact "build project generates expected working_dir output"
      (let [workdir-dir (fs/file "dev-resources/projects/aleph_default/working_dir")
            backup-dir (fs/file "dev-resources/projects/aleph_default/working_dir_orig")]
        (try
          (core/move-file workdir-dir backup-dir)
          (fs/mkdir workdir-dir)
          (let [app (core/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/"))
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

(future-fact "provide [apply] button for settings, settings until 'applied' are stored locally on screen")