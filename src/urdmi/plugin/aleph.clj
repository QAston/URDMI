(ns urdmi.plugin.aleph
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [urdmi.prolog :as prolog])
  (:import (java.io StringReader)))

(defn run [project-settings]
  (let [swiprolog-location "C:\\Program Files\\pl\\bin\\plcon.exe"
        aleph-location "C:\\portable\\aleph.pl"
        working-dir (io/file (io/resource "projects/aleph_default/working_dir"))
        dbname "pracownik"]
    (shell/sh swiprolog-location "-g" "true"
               :in (StringReader. (str "consult('" (prolog/quote aleph-location) "').\nread_all(" (prolog/quote dbname) ").\n.\ninduce.\nhalt.\n"))
              :dir working-dir
              )))