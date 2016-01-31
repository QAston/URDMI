(ns urdmi.core-test
  (:use midje.sweet
        urdmi.prolog-test)
  (:require
    [urdmi.core :as core]
    [clojure.java.io :as io]
    [me.raynes.fs :as fs]))

(fact "relativize path correctly calls Path relativize"
      (core/relativize-path (fs/file ".") (fs/file "dev-resources")) => (io/file "dev-resources"))

(fact "relation-to-filename"
      (core/relation-to-filename ["rel" 5]) => "rel_5.pl")

(facts 'core/merge-diff
       (core/merge-diff (core/->ModelDiff nil nil) nil) => (core/->ModelDiff nil nil)
       (core/merge-diff nil (core/->ModelDiff nil nil)) => (core/->ModelDiff nil nil)
       (core/merge-diff nil nil)
       (core/merge-diff (core/->ModelDiff [[[:prolog-ext "a"] "a"]
                                           [[:prolog-ext "c"] "c"]
                                           [[:prolog-ext "e"] "e"]] [[:prolog-ext "b"]
                                                                     [:prolog-ext "d"]])
                        (core/->ModelDiff [[[:prolog-ext "a"] "anew"]
                                           [[:prolog-ext "d"] "d"]] [[:prolog-ext "c"]])) => (core/->ModelDiff [[[:prolog-ext "e"] "e"]
                                                                                                                [[:prolog-ext "a"] "anew"]
                                                                                                                [[:prolog-ext "d"] "d"]] [[:prolog-ext "b"] [:prolog-ext "c"]]))