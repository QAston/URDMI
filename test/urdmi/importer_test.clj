(ns urdmi.importer-test
  (:require [me.raynes.fs :as fs]
            [urdmi.prolog :as prolog]))

(use 'urdmi.importer)
(use 'midje.sweet)

(facts
  (keys (load-from-location (fs/file "dev-resources/projects/aleph_default/relations/dzial_6.pl")
                   (prolog/aleph-parser-context)))
  =>
  [["dzial" 6]]

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
    (count (get merged ["gfather" 2]))=> 6))
