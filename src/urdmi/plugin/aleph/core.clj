(ns urdmi.plugin.aleph.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [urdmi.prolog :as prolog]
            [urdmi.core :as api]
            [clojure.zip :as zip]
            [urdmi.core :as core]
            [me.raynes.fs :as fs]
            [clojure.set :as set])
  (:import (java.io StringReader IOException)
           (urdmi.core Project)))

(use 'clojure.pprint)

(def settings-filename "aleph.edn")
(def hypothesis-name "hypothesis.edn")
(def datamining-name "datamining.edn")

(def programs #{"induce" "induce_cover" "induce_max" "induce_incremental"
                "induce_clauses" "induce_theory" "induce_tree" "induce_constraints" "induce_modes" "induce_features"
                "custom"})

(defn check-plcon-path [resolved-loc]
  (try
    (when-let [o (:err (shell/sh (str resolved-loc)
                                 :in (StringReader. "")
                                 ))]
      (.startsWith o "Welcome to SWI-Prolog"))
    (catch IOException e
      false)))

(defn split-by-relation-arg [rel-asts rel-arg]
  ;convert prolog-atoms to int
  (let [grouped-rel (into {} (map (fn [[k v]]
                                    [(:value k) v]) (prolog/extract-relation-arg rel-asts rel-arg)))]
    [(sort-by hash (get grouped-rel 1)) (sort-by hash (get grouped-rel 0))]))

(defn get-advanced-example-data-settings [relation term ^String true-val ^String false-val]
  [{:value         true-val
    :value-type    :positive
    :relation      relation
    :relation-term term}
   {:value         false-val
    :value-type    :negative
    :relation      relation
    :relation-term term}])

(defn get-learning-examples-settings [^Project p]
  (let [example-data (:example (api/get-settings-data p datamining-name))]
    (if (= :simple (:type example-data))
      (get-advanced-example-data-settings (:relation example-data) (:term example-data) (:true-val example-data) (:false-val example-data))
      (:advanced-list example-data))))

(defn get-background-relations [^Project p]
  (let [background-data (:background (api/get-settings-data p datamining-name))
        learning-example (get-learning-examples-settings p)
        relation-list (:relation-list background-data)]
    (sort (vec
             (condp = (:type background-data)
                  :all-but-example (set/difference (set (map :rel (api/get-relations p))) (set (map :relation learning-example)))
                  :all (map :rel (api/get-relations p))
                  :selected relation-list
                  )))))

(defn get-clause-settings [^Project p]
  (let [clause-settings-data (:clause (api/get-settings-data p hypothesis-name))]
    clause-settings-data))

(defn generate-hypothesis-settings-from-learning-example-settings [^Project p]
  (let [learning-examples (get-learning-examples-settings p)
        available-clauses (set (map :relation (:clause-list (get-clause-settings p))))
        head-clauses (map :relation learning-examples)]
    (into {} (->>
               (for [head-clause head-clauses]
                [head-clause (vec (sort (vec (disj available-clauses head-clause))))])
               (filter (fn [[f s]]
                         (not-empty s)))))))

(defn get-hypothesis-settings [^Project p]
  (let [hypothesis-settings-data (:hypothesis (api/get-settings-data p hypothesis-name))
        ]
    (if (:autogenerate-hypothesis hypothesis-settings-data)
      (generate-hypothesis-settings-from-learning-example-settings p)
      (:hypothesis-list hypothesis-settings-data)
      )))

(defn get-training-examples [^Project project]
  (let [plugin-settings (api/get-settings-data project settings-filename)
        target-relation (:target-rel plugin-settings)
        target-relation-param (:target-rel-param plugin-settings)
        target-rel-asts @(:data (api/get-relation project target-relation))
        ]
    (split-by-relation-arg target-rel-asts target-relation-param)
    ))

(defn get-db-name [^Project p]
  "db")

(defn term-spec-to-string [{:keys [type value] :as term-spec}]
  (str type value))

(defn build-b-file [plugin ^Project project]
  (let [
        parser-context (core/get-parser-context plugin)
        plugin-settings (api/get-settings-data project settings-filename)
        working-dir (api/get-working-dir project)
        background-relations (sort (vec (disj (set (map :rel (api/get-relations project))) (:target-rel plugin-settings))))
        filename (str (get-db-name project) ".b")
        ]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (doseq [rel background-relations]
        (let [ast @(:data (api/get-relation project rel))]
          (.append writer (str "\n % relation: " (api/relation-to-string rel) "\n"))
          (prolog/pretty-print-sentences parser-context ast writer)
          ))
      (api/try-append-prolog-ext-file project (io/file "bg_and_settings.pl") writer))))

(defn build-f-file [plugin ^Project project]
  (let [working-dir (api/get-working-dir project)
        asts (first (get-training-examples project))
        parser-context (core/get-parser-context plugin)
        filename (str (get-db-name project) ".f")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context asts writer)
      (api/try-append-prolog-ext-file project (io/file "positive_examples.pl") writer))))

(defn build-n-file [plugin ^Project project]
  (let [working-dir (api/get-working-dir project)
        asts (second (get-training-examples project))
        parser-context (core/get-parser-context plugin)
        filename (str (get-db-name project) ".n")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (prolog/pretty-print-sentences parser-context asts writer)
      (api/try-append-prolog-ext-file project (io/file "negative_examples.pl") writer))))

(defn- validate-settings [project key]
  (let [plugin-settings (api/get-settings-data project settings-filename)]
    (not (and (core/check-relation-term project [(:target-rel plugin-settings) (:target-rel-param plugin-settings)])
              (:program plugin-settings)
              (let [aleph-loc (fs/file (core/resolve-relative-loc (:project-dir project) (:aleph-loc plugin-settings)))
                    swi-prol-loc (core/resolve-executable-loc (:project-dir project) (:swi-prolog-loc plugin-settings))]
                (and (fs/exists? aleph-loc)
                     (check-plcon-path swi-prol-loc)))))))

(defrecord AlephPlugin [parser-context]
  api/Plugin
  (run [this project]
    (let [plugin-settings (api/get-settings-data project settings-filename)
          swiprolog-location (core/resolve-executable-loc (:project-dir project) (:swi-prolog-loc plugin-settings))
          aleph-location (core/resolve-relative-loc (:project-dir project) (:aleph-loc plugin-settings))
          working-dir (api/get-working-dir project)
          dbname (get-db-name project)
          learning-program (get plugin-settings :program "induce")
          learning-program (if (= learning-program "custom")
                             (slurp (io/file (core/get-prolog-ext-dir project) "custom_program.pl"))
                             (str learning-program ".\n"))]
      (shell/sh (str swiprolog-location) "-g" "true"
                :in (StringReader. (str "consult('" (prolog/quote-atom (str aleph-location)) "').\nread_all(" (prolog/quote-atom dbname) ").\n.\n" learning-program "\nhalt.\n"))
                :dir working-dir
                )))
  (rebuild-working-dir [this project]
    (build-b-file this project)
    (build-f-file this project)
    (build-n-file this project))
  (get-parser-context [this]
    parser-context
    )
  (generate-output [this project run-result])
  (model-created [this project]
    (core/->ModelDiff [[[:prolog-ext "negative_examples.pl"] (core/file-item "%negative examples \n% file appended to the generated .n file")]
                       [[:prolog-ext "positive_examples.pl"] (core/file-item "%positive examples \n% file appended to the generated .f file")]
                       [[:prolog-ext "bg_and_settings.pl"] (core/file-item "%background knowledge and aleph settings \n% file appended to the generated .b file")]
                       [[:prolog-ext "custom_program.pl"] (core/file-item "%custom program run instead of the default induce_* command. can be enabled in aleph settings.")]
                       [[:settings settings-filename] (core/file-item
                                                        {:target-rel       nil
                                                         :target-rel-param nil
                                                         :aleph-loc        ""
                                                         :swi-prolog-loc   "plcon"
                                                         :program          "induce"
                                                         })]] []))
  (model-loaded [this project])
  (model-modified [this project key])
  (is-model-invalid [this project key]
    (condp = key
      [:settings settings-filename] (validate-settings project key)
      false)))

(defn create []
  (->AlephPlugin (prolog/aleph-parser-context)))