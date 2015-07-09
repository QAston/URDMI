(ns urdmi.core-test
  (:use midje.sweet)
  (:import java.io.StringReader
           (com.ugos.jiprolog.engine PrologObject))
  (:require
    [urdmi.core :as core]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [me.raynes.fs :as fs]))

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

