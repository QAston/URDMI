(ns urdmi.importer-test
  (:require [me.raynes.fs :as fs]
            [urdmi.prolog :as prolog]
            [urdmi.app :as app]
            [urdmi.core :as core]))

(use 'urdmi.importer)
(use 'midje.sweet)
(use 'urdmi.prolog-test)

(facts 'load-from-location
       (let [import-data (load-from-location (fs/file "dev-resources/projects/aleph_default/relations/dzial_6.pl")
                                 (prolog/aleph-parser-context))]
         (keys import-data) => [["dzial" 6]]
         )


       (let [merged (load-from-location (fs/file "dev-resources/aleph_default/") (prolog/ace-parser-context))]
         (keys merged) => (list ["pracownikprodukcja" 6]
                                ["pracownikpersonalia" 8]
                                ["pracownik" 6]
                                ["father" 2]
                                ["mother" 2]
                                ["zamowienieszczegoly" 4]
                                ["produkcja" 5]
                                ["dzial" 6]
                                ["towar" 6]
                                ["gfather" 2]
                                ["klient" 9]
                                ["zamowienie" 5])
         (count (get merged ["gfather" 2])) => 6
         (get-in merged [["gfather" 2] 5]) => {:children (list {:name "gfather", :type :ast-atom} {:type :ast-expression, :value 6} {:type :ast-expression, :value 5}), :type :ast-functor}
         )
       )

(facts 'generate-model-diff-for-project-import
       (let [parser-context (prolog/aleph-parser-context)
             project (:project (app/load-project (app/init-app) (fs/file "dev-resources/projects/aleph_default/")))
             import-data {["dzial" 6]
                          [(first (parse-string parser-context "dzial(8,informatyka,produkcyjna,1,1,lapy)."))
                           (first (parse-string parser-context "dzial(9,nowy,produkcyjna,1,1,lapy)."))]}
             merged-project (core/apply-diff project (generate-model-diff-for-project-import project import-data))
             relation-data (:data (core/get-relation merged-project ["dzial" 6]))]
         (nth @relation-data 7) => {:children (list {:name "dzial", :type :ast-atom} {:type :ast-expression, :value 8} {:name "informatyka", :type :ast-atom} {:name "produkcyjna", :type :ast-atom} {:type :ast-expression, :value 1} {:type :ast-expression, :value 1} {:name "lapy", :type :ast-atom}), :type :ast-functor}
         (get @relation-data 8) => {:children (list {:name "dzial", :type :ast-atom} {:type :ast-expression, :value 9} {:name "nowy", :type :ast-atom} {:name "produkcyjna", :type :ast-atom} {:type :ast-expression, :value 1} {:type :ast-expression, :value 1} {:name "lapy", :type :ast-atom}), :type :ast-functor}
         ))
