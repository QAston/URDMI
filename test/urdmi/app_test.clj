(ns urdmi.app-test
  (:use midje.sweet)
  (:use urdmi.app)
  (:require [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [clojure.java.io :as io])
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
        (get-in settings ["aleph.edn" :data]) => {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 6}
        (core/get-settings-data (:project app) "aleph.edn") =>  {:aleph-loc "C:\\portable\\aleph.pl", :swi-prolog-loc "C:\\Program Files\\pl\\bin\\plcon.exe", :target-rel ["pracownik" 7], :target-rel-param 6}
        (extends? core/Plugin (class (get-in app [:project :plugin]))) => truthy))

(fact "load project populates project fields"
      (let [proj (:project (core/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/")))]
        (< 0 (count (get-in proj (core/dir-keys core/relations-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/additions-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/workdir-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/output-keyname :dir)))) => truthy
        (< 0 (count (get-in proj (core/dir-keys core/settings-keyname :dir)))) => truthy))

(defn alter-working-dir[^App app ^File dir]
  (assoc-in app [:project :dir :settings :dir "project.edn" :data :working-dir] dir ))

(fact "build project generates expected working_dir output"
             (let [app (core/load-project (init-app) (fs/file "dev-resources/projects/aleph_default/"))
                   app (alter-working-dir app (io/file "workdir_test"))]
               (core/get-working-dir (:project app)) => (fs/file "dev-resources/projects/aleph_default/workdir_test")
               (build-working-dir (:project app))))

(future-fact "provide [apply] button for settings, settings until 'applied' are stored locally on screen")