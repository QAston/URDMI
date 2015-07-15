(ns urdmi.core-test
  (:use midje.sweet)
  (:import java.io.StringReader
           (com.ugos.jiprolog.engine PrologObject))
  (:require
    [urdmi.core :as core]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [me.raynes.fs :as fs]
    [clojure.edn :as edn]
    [clojure.zip :as zip]))

(facts "merge addition works"
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
            rels (map second (get-in loaded (core/dir-keys core/relations-keyname :dir)))
            relkeys (map first (get-in loaded (core/dir-keys core/relations-keyname :dir)))]
        (map :name rels) => (just #{"towar_6.pl" "produkcja_5.pl" "pracownik_7.pl" "pracownikpersonalia_8.pl" "klient_9.pl" "zamowienieszczegoly_4.pl" "pracownikprodukcja_7.pl" "zamowienie_5.pl" "dzial_6.pl"})
        (map :rel rels) => (just #{["dzial" 6] ["klient" 9] ["pracownik" 7] ["pracownikpersonalia" 8] ["pracownikprodukcja" 7] ["produkcja" 5] ["towar" 6] ["zamowienie" 5] ["zamowienieszczegoly" 4]})
        relkeys => (just #{"towar_6.pl" "produkcja_5.pl" "pracownik_7.pl" "pracownikpersonalia_8.pl" "klient_9.pl" "zamowienieszczegoly_4.pl" "pracownikprodukcja_7.pl" "zamowienie_5.pl" "dzial_6.pl"})))


(fact "load working dir loads base working dir data from disk"
      (let [base (core/load-working-dir (core/base-project (fs/file "dev-resources/projects/ace_tilde/")))
            tildedir (get-in base (core/dir-keys core/workdir-keyname "tilde"))
            outfile (get-in base (core/dir-keys core/workdir-keyname "tilde" "pracownik.out"))]
        (:name tildedir) => "tilde"
        (:name outfile) => "pracownik.out"
        (map first (:dir tildedir)) => (just #{"pracownik.out" "pracownik.progress" "pracownik.ptree"})))

(fact "file-model-zipper allows moving among files"
      (let [base (core/base-project (fs/file "dev-resources/projects/ace_tilde/"))
            workdir (core/workdir-keyname (:dir (core/load-working-dir base)))]
        (zip/node (zip/down (core/file-model-zipper workdir))) => truthy
        (get (:dir (zip/root (zip/append-child (core/file-model-zipper workdir) {:name "tyry"}))) "tyry") => {:name "tyry"}
        ))

(fact "load additions loads additions data from disk"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            additions (get-in (core/load-additions base) (core/dir-keys core/additions-keyname :dir))]
        (map first additions) => (just #{"pracownik.b"})))

(fact "load output"
      (let [base (core/base-project (fs/file "dev-resources/projects/aleph_default/"))
            additions (get-in (core/load-output base) (core/dir-keys core/output-keyname :dir))]
        (map first additions) => (just #{"result.edn"})))

(fact "relation-to-filename"
      (core/relation-to-filename ["rel" 5]) => "rel_5.pl")

(future-fact "generate-menu-entries works on example data"
      )