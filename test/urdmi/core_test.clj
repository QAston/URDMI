(ns urdmi.core-test
  (:use midje.sweet
        urdmi.prolog-test)
  (:require
    [urdmi.core :as core]
    [clojure.java.io :as io]
    [me.raynes.fs :as fs]))

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

(fact "relation-to-filename"
      (core/relation-to-filename ["rel" 5]) => "rel_5.pl")