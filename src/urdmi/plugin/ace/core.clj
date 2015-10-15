(ns urdmi.plugin.ace.core
  (:require [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [urdmi.core :as api]
            [urdmi.prolog :as prolog]
            [urdmi.core :as core])
  (:import (java.io StringReader)
           (urdmi.core Project)))

(def settings-filename "ace.edn")

(def knowledgebase-models "models")
(def knowledgebase-key "key")

(def example-commands #{"induce(tilde)" "induce(icl)" "induce(regrules)" "induce(Algo)" "induce(bagging(BasicAlgo, n))" "induce(boosting(BasicAlgo, n))" "induce(voting(BasicAlgo, n))" "warmr" "rrl" "mrrl(N)" "nfold(Algo,n)" "nfold(Algo,n,s)" "leave_one_out_from_list(Algo,list)"})

(defn get-app-name [^Project p]
  (first (:target-rel (api/get-settings-data p settings-filename))))

(defn relations-to-model-format [relations-map model-relation model-relation-index included-relations]
  (let [indexed-model-relaton (prolog/extract-relation-arg (get relations-map model-relation) model-relation-index)

        indexed-relations (keep identity
                                (for [[[relation-name relation-arity :as relation] relation-term-idx] included-relations]
                                  (when-let [sentences (get relations-map relation)]
                                    (prolog/extract-relation-arg sentences relation-term-idx))))
        ]
    (apply concat
           (for [[index-value sentences] indexed-model-relaton]
             (let [model-idx-node {:children (list {:name "model", :type :ast-atom}
                                                   index-value),
                                   :type     :ast-functor}
                   begin-sentence {:children (list {:name "begin", :type :ast-atom}
                                                   model-idx-node),
                                   :type     :ast-functor}
                   end-sentence {:children (list {:name "end", :type :ast-atom}
                                                 model-idx-node),
                                 :type     :ast-functor}
                   relation-sentences (mapcat #(get % index-value) indexed-relations)
                   ]
               (conj (into [begin-sentence] (concat sentences relation-sentences)) end-sentence)
               )))))

(defn- build-bg-knowledge-file [plugin project]
  (let [
        parser-context (core/get-parser-context plugin)
        plugin-settings (api/get-settings-data project settings-filename)
        working-dir (api/get-working-dir project)
        background-relations (sort (vec (disj (set (map :rel (api/get-relations project))) (:target-rel plugin-settings))))
        filename (str (get-app-name project) ".bg")
        ]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (doseq [rel background-relations]
        (let [ast @(:data (api/get-relation project rel))]
          (.append writer (str "\n % relation: " (api/relation-to-string rel) "\n"))
          (prolog/pretty-print-sentences parser-context ast writer)
          ))
      (api/try-append-prolog-ext-file project (io/file "bg.pl") writer))))

(defn get-kb-format [settings]
  (get settings :kb-format knowledgebase-key))

(defn- build-knowledge-base-file [plugin project]
  (let [working-dir (api/get-working-dir project)
        plugin-settings (api/get-settings-data project settings-filename)
        target-rel-asts @(:data (api/get-relation project (:target-rel plugin-settings)))
        parser-context (core/get-parser-context plugin)
        filename (str (get-app-name project) ".kb")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (if (= (get-kb-format plugin-settings) knowledgebase-key)
        (prolog/pretty-print-sentences parser-context target-rel-asts writer)
        (let [relations-map (into {} (map (fn [e] [(:rel e) @(:data e)]) (api/get-relations project)))
              models-format (get plugin-settings :models-format {:target-relation-index 0
                                                                 :joined-relations      []})
              target-rel-index (:target-relation-index models-format)
              joinded-rels (:joined-relations models-format)

              model-format-relations (relations-to-model-format relations-map (:target-rel plugin-settings) target-rel-index joinded-rels)]
          (prolog/pretty-print-sentences parser-context model-format-relations writer)

          ))
      (api/try-append-prolog-ext-file project (io/file "kb.pl") writer))))

(defn- build-settings-file [plugin project]
  (let [working-dir (api/get-working-dir project)
        plugin-settings (api/get-settings-data project settings-filename)
        filename (str (get-app-name project) ".s")]
    (with-open [writer (io/writer (io/file working-dir filename))]
      (binding [*out* writer]
        (println (str "load(" (get-kb-format plugin-settings) ")."))
        )
      (api/try-append-prolog-ext-file project (io/file "settings.pl") writer))))

(defrecord AcePlugin [parser-context]
  api/Plugin
  (run [this project]
    (let [
          plugin-settings (api/get-settings-data project settings-filename)
          ace-location (:ace-loc plugin-settings)
          command (:command plugin-settings)
          working-dir (api/get-working-dir project)]
      (shell/sh ace-location
                :in (StringReader. command)
                :dir working-dir
                ))
    )
  (rebuild-working-dir [this project]
    (build-bg-knowledge-file this project)
    (build-knowledge-base-file this project)
    (build-settings-file this project))
  (get-parser-context [this]
    parser-context
    )
  (generate-output [this project run-result])
  (model-created [this project]
    (core/->ModelDiff [[[:prolog-ext "bg.pl"] (core/file-item "% background knowledge \n% file appended to the generated .bg file")]
                       [[:prolog-ext "kb.pl"] (core/file-item "% examples\n% file appended to the generated .kb file")]
                       [[:prolog-ext "settings.pl"] (core/file-item "% ace engine settings\n% file appended to the generated .s file")]
                       [[:settings settings-filename] (core/file-item {:target-rel    nil
                                                                       :ace-loc       ""
                                                                       :kb-format     knowledgebase-key
                                                                       :models-format {:target-relation-index 0
                                                                                       :joined-relations      []}})]] []))
  (model-loaded [this project])
  (model-modified [this project key]))

(defn create []
  (->AcePlugin (prolog/ace-parser-context)))