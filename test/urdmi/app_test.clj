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

(defn base-app [dir]
  (->
    (app/init-app)
    (assoc :project (core/base-project dir))
    (load-settings)))

(fact "load-relations loads base relation data from disk"
      (let [loaded (->
                     (base-app (fs/file "dev-resources/projects/aleph_default/"))
                     (app/load-relations)
                     (:project))
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
            relkeys (map first (get-in loaded (core/model-map-keys core/relations-keyname :dir)))]
        (map :name rels) => (just #{"towar_6.pl" "produkcja_5.pl" "pracownik_7.pl" "pracownikpersonalia_8.pl" "klient_9.pl" "zamowienieszczegoly_4.pl" "pracownikprodukcja_6.pl" "zamowienie_5.pl" "dzial_6.pl"})
        (map :rel rels) => (just #{["dzial" 6] ["klient" 9] ["pracownik" 7] ["pracownikpersonalia" 8] ["pracownikprodukcja" 6] ["produkcja" 5] ["towar" 6] ["zamowienie" 5] ["zamowienieszczegoly" 4]})
        @(:data (core/get-relation loaded ["dzial" 6])) => dzial-6-rel
        relkeys => (just #{"towar_6.pl" "produkcja_5.pl" "pracownik_7.pl" "pracownikpersonalia_8.pl" "klient_9.pl" "zamowienieszczegoly_4.pl" "pracownikprodukcja_6.pl" "zamowienie_5.pl" "dzial_6.pl"})))


(fact "load working dir loads base working dir data from disk"
      (let [base (-> (base-app (fs/file "dev-resources/projects/ace_tilde/"))
                     (app/load-working-dir)
                     (:project))
            tildedir (get-in base (core/model-map-keys core/workdir-keyname "tilde"))
            outfile (get-in base (core/model-map-keys core/workdir-keyname "tilde" "pracownik.out"))]
        (:name tildedir) => "tilde"
        (:name outfile) => "pracownik.out"
        (map first (:dir tildedir)) => (just #{"pracownik.out" "pracownik.progress" "pracownik.ptree"})))

(fact "file-model-zipper allows moving among files"
      (let [base (-> (base-app (fs/file "dev-resources/projects/ace_tilde/"))
                     (app/load-working-dir)
                     (:project))
            workdir (core/workdir-keyname (:dir base))]
        (zip/node (zip/down (core/file-model-zipper workdir))) => truthy
        (get (:dir (zip/root (zip/append-child (core/file-model-zipper workdir) {:name "tyry"}))) "tyry") => {:name "tyry"}
        ))

(fact "load-prolog-ext loads prolog data from disk"
      (let [base (-> (base-app (fs/file "dev-resources/projects/aleph_default/"))
                     (app/load-prolog-ext)
                     (:project))
            items (get-in base (core/model-map-keys core/prolog-ext-keyname :dir))]
        (map first items) => (just #{"negative_examples.pl" "bg_and_settings.pl" "positive_examples.pl" "custom_program.pl"})))

(fact "load output"
      (let [base (-> (base-app (fs/file "dev-resources/projects/aleph_default/"))
                     (app/load-output)
                     (:project))
            items (get-in base (core/model-map-keys core/output-keyname :dir))]
        (map first items) => (just #{"result.edn"})))

(fact "init-app loads plugins"
      (let [app (init-app)]
        (map first (:plugins app)) => (just #{:ace :aleph})))

(fact "load settings"
      (let [app (base-app (fs/file "dev-resources/projects/aleph_default/"))
            settings (get-in (:project app) (core/model-map-keys core/settings-keyname :dir))]
        (map first settings) => (just #{"project.edn" "aleph.edn" "datamining.edn" "hypothesis.edn"})
        @(get-in settings ["project.edn" :data]) => {:working-dir (io/file "build_dir") :active-plugin :aleph}
        @(get-in settings ["aleph.edn" :data]) => {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 6}
        (core/get-settings-data (:project app) "aleph.edn") => {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 6}
        (extends? core/Plugin (class (get-in app [:project :plugin]))) => truthy))

(fact "load project populates project fields"
      (let [proj (:project (app/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/")))]
        (< 0 (count (get-in proj (core/model-map-keys core/relations-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/model-map-keys core/prolog-ext-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/model-map-keys core/workdir-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/model-map-keys core/output-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/model-map-keys core/settings-keyname :dir)))) => truthy))

(facts get-model-item-keys
       (fact "returns file-name-keys for example project"
             (let [app (app/load-project (init-app) (fs/file "dev-resources/projects/ace_tilde/"))]
               (get-model-item-keys (:project app))) => (just #{[:prolog-ext "settings.pl"]
                                                                [:prolog-ext "bg.pl"]
                                                                [:prolog-ext "kb.pl"]
                                                                [:settings "project.edn"]
                                                                [:settings "ace.edn"]
                                                                [:working-dir "pracownik.bg.w"]
                                                                [:working-dir "pracownik.kb"]
                                                                [:working-dir "pracownik.s"]
                                                                [:working-dir "pracownik.bg"]
                                                                [:working-dir ".pracownik.keycode"]
                                                                [:working-dir "pracownik.kb.w"]
                                                                [:working-dir ".pracownik.keycode.w"]
                                                                [:working-dir "tilde" "pracownik.ptree"]
                                                                [:working-dir "tilde" "pracownik.progress"]
                                                                [:working-dir "tilde" "pracownik.out"]
                                                                [:relations "towar_6.pl"]
                                                                [:relations "pracownikprodukcja_6.pl"]
                                                                [:relations "produkcja_5.pl"]
                                                                [:relations "pracownik_7.pl"]
                                                                [:relations "pracownikpersonalia_8.pl"]
                                                                [:relations "klient_9.pl"]
                                                                [:relations "zamowienieszczegoly_4.pl"]
                                                                [:relations "zamowienie_5.pl"]
                                                                [:relations "dzial_6.pl"]})))

(fact get-model-dirs
      (let [proj-dir (fs/file "dev-resources/projects/ace_tilde/")
            app (app/load-project (init-app) proj-dir)]
        (get-model-dirs (:project app)) => (just #{(fs/file proj-dir "prolog-ext")
                                                   (fs/file proj-dir "relations")
                                                   (fs/file proj-dir "settings")
                                                   (fs/file proj-dir "working_dir")
                                                   (fs/file proj-dir "output")
                                                   })))

(defn dir-equals [proj-a proj-b]
  (let [files (get-model-item-keys proj-a)]
    (doseq [file files]
      (with-open [file-a (io/reader (core/item-key-to-file proj-a file))
                  file-b (io/reader (core/item-key-to-file proj-b file))]
        (while (let [a (.read file-a)
                     b (.read file-b)]
                 (when-not (= a b)
                   (throw (Exception. (str "files" (core/item-key-to-file proj-a file) (core/item-key-to-file proj-b file) " differ"))))
                 (not= a -1)))))))

(fact "saving a copy of project in another place produces an exact copy"
      (let [copy-proj-dir (fs/file "dev-resources/projects/ace_tilde_copy/")]
        (try
          (let [orig-app (app/load-project (init-app) (fs/file "dev-resources/projects/ace_tilde/"))
                _ (fs/mkdir copy-proj-dir)
                app (assoc-in orig-app [:project :project-dir] copy-proj-dir)
                app (save-files app (get-model-item-keys (:project app)))
                copy-app (app/load-project (init-app) copy-proj-dir)]
            (get-model-item-keys (:project orig-app)) => (get-model-item-keys (:project copy-app))
            (dir-equals (:project orig-app) (:project copy-app))
            )
          (finally
            (fs/delete-dir copy-proj-dir)))
        ))

(fact core/file-to-item-key
      (let [proj-dir (fs/file "dev-resources/projects/ace_tilde/")
            app (app/load-project (init-app) proj-dir)]
        (core/file-to-item-key (:project app) (fs/file "dev-resources/projects/ace_tilde/relations/dzial_6.pl")) => [:relations "dzial_6.pl"]))

(fact core/generate-relation-term-values-map
      (let [proj-dir (fs/file "dev-resources/projects/ace_tilde/")
            app (app/load-project (init-app) proj-dir)]
        (get-in (core/generate-relation-term-values-map (:project app)) [["pracownik" 7] 6]) =>  #{"0" "1"}))
