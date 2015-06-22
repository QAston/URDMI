(ns urdmi.prolog-test
  (:use midje.sweet)
  (:import java.io.StringReader
           (com.ugos.jiprolog.engine PrologObject))
  (:require
    [urdmi.prolog :as prolog]
    [clojure.java.io :as io]))

(fact "prolog expr seq returns a seq of ast nodes"
      (let [empty (prolog/prolog-sentence-seq (StringReader. ""))
            single-sentence (prolog/prolog-sentence-seq (StringReader. "hello :- world."))
            two-sentences (prolog/prolog-sentence-seq (StringReader. "hello :- world. parent(a):- mother."))
            ]
        (count single-sentence) => 1
        (count two-sentences) => 2
        (count empty) => 0
        (instance? PrologObject (first two-sentences)) => true
        )
      )

(defn get-prolog-files-for-tests []
  (vec (flatten (for [extension ["pl" "kb" "bg" "b" "n" "f" "s"]]
                  (let [paths (map (memfn toPath) (file-seq (io/file "dev-resources/")))]
                    (->> (filter #(.endsWith (str %) (str "." extension)) paths)
                         (map (fn [path]
                                (.subpath path 1 (.getNameCount path))))
                         (map str))
                    )))))

(fact "parser can parse example prolog files"
      (let [files (get-prolog-files-for-tests)]
         (doseq [file files]
           (fact file
                 (try (count (prolog/prolog-sentence-seq
                           (io/make-reader
                             (io/resource file) {})))
                      (catch Exception e
                        (throw (Exception. (str "Couldn't parse " file) e)))) => anything)
           )))

