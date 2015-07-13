(ns urdmi.app-test
  (:use midje.sweet)
  (:use urdmi.app)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]))

(fact "init-app loads plugins"
      (let [app (init-app)]
        (map first (:plugins app)) => (just #{:ace :aleph})))

(future-fact "build project generates expected working_dir output"
             (let [proj (core/load-project (fs/file "dev-resources/projects/aleph_default/"))]
               (core/build proj)))