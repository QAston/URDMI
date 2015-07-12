(ns urdmi.core-test
  (:use midje.sweet)
  (:import java.io.StringReader
           (com.ugos.jiprolog.engine PrologObject))
  (:require
    [urdmi.core :as core]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [me.raynes.fs :as fs]))

#_(facts "merge addition works"
       (let [additions-dir-name (fs/file "dev-resources/temp/proj/additions")
             working-dir-name (fs/file "dev-resources/temp/proj/working")
             working-file-appended (io/file "subdir/appended")
             addition-file-new (io/file "new")
             addition-file-appended (io/file "subdir/appended")
             spit-file (fn [dir name]
                         (fs/mkdirs (fs/parent (fs/file dir name)))
                         (spit (fs/file dir name) (str dir "/" name))
                         (str dir "/" name))
             slurp-file (fn [name]
                          (slurp (fs/file working-dir-name name)))]
         (try (fact " dir created " (fs/mkdirs additions-dir-name) => truthy)
              (let [new-file-content (spit-file additions-dir-name addition-file-new)
                    append-file-content (str (spit-file working-dir-name working-file-appended) (spit-file additions-dir-name addition-file-appended))]

                (fact "new file is created in working dir if didn't exist"
                      (core/merge-addition  working-dir-name additions-dir-name addition-file-new)
                      (slurp-file addition-file-new) => new-file-content)
                (fact "already existing file has appendix added to it"
                      (core/merge-addition  working-dir-name additions-dir-name working-file-appended)
                      (slurp-file working-file-appended) => append-file-content)

                )
              (finally (fs/delete-dir "dev-resources/temp/"))))
       )

(fact "relativize path correctly calls Path relativize"
      (core/relativize-path (fs/file ".") (fs/file "dev-resources")) => (io/file "dev-resources"))

(fact "iterate-subdir dirs do not have parent path"
      (let [thisdir (.toString (fs/file "."))]
        (->> (core/iterate-subdir (fs/file "."))
             (map #(.toString (first %)))
             (not-any? #(.startsWith % thisdir))) => truthy
        (->> (core/iterate-subdir (fs/file "."))
             (map #(.toString (first %)))
             (every? #(.startsWith % "."))) => truthy
        ))

(fact "load-relations loads base relation data from disk"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            loaded (core/load-relations base)
            rels (map second (core/relations-keyname loaded))
            relkeys (map first (core/relations-keyname loaded))]
        (map :filename rels) => (just #{"dzial.pl" "klient.pl" "pracownik.pl" "pracownikpersonalia.pl" "pracownikprodukcja.pl" "produkcja.pl" "towar.pl" "zamowienie.pl" "zamowienieszczegoly.pl"})
        (map :name rels) => (just #{"dzial" "klient" "pracownik" "pracownikpersonalia" "pracownikprodukcja" "produkcja" "towar" "zamowienie" "zamowienieszczegoly"})
        relkeys => (just #{"dzial" "klient" "pracownik" "pracownikpersonalia" "pracownikprodukcja" "produkcja" "towar" "zamowienie" "zamowienieszczegoly"})))


(fact "load working dir loads base working dir data from disk"
      (let [base (core/base-project (fs/file "dev-resources/projects/ace_tilde/"))
            workdir (core/workdir-keyname (core/load-working-dir base))
            tildedir (get-in workdir ["tilde"])
            outfile (get-in workdir ["tilde" :dir "pracownik.out"])]
        (:name tildedir) => "tilde"
        (:name outfile) => "pracownik.out"
        (map first (:dir tildedir)) => (just #{"pracownik.out" "pracownik.progress" "pracownik.ptree"})))

(fact "load additions loads additions data from disk"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            additions (core/additions-keyname (core/load-additions base))]
        (map first additions) => (just #{"pracownik.b"})))

(fact "load settings"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            additions (core/settings-keyname (core/load-settings base))]
        (map first additions) => (just #{"project.edn" "aleph.edn"})))

(fact "load output"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            additions (core/output-keyname (core/load-output base))]
        (map first additions) => (just #{"result.edn"})))

(fact "load project populates project fields"
      (let [proj (core/load-project (fs/file "dev-resources/projects/aleph_default/"))]
        (< 0 (count (core/relations-keyname proj))) => truthy
        (< 0 (count (core/additions-keyname proj))) => truthy
        (< 0 (count (core/output-keyname proj))) => truthy
        (< 0 (count (core/settings-keyname proj))) => truthy
        (< 0 (count (core/workdir-keyname proj))) => truthy))

(fact "generate-menu-entries works on example data"
      (let [proj (core/load-project (fs/file "dev-resources/projects/aleph_default/"))]
        (core/generate-menu-entries proj) => []))