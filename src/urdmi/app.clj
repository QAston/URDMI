(ns urdmi.app
  "stuff depending both on core and plugins namespace
  mainly plugin loading and app init."
  (:use urdmi.core)
  (:require [urdmi.plugin.ace :as ace]
            [urdmi.plugin.aleph :as aleph]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
            [urdmi.prolog :as prolog]
            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [urdmi.core :as core])
  (:import (urdmi.core App Project)
           (java.io File Reader Writer)))

;plugins should be loaded from a ./settings/plugins dir
(defn register-plugins [^App app]
  (-> app
      (register-plugin :ace #'ace/create)
      (register-plugin :aleph #'aleph/create)))

;loading files
(defn- generate-model-map
  [dir root-node-name]
  (let [subdir-files (map
                       (fn [subdir]
                         (let [path-parts (vec (iterator-seq (.iterator (.toPath (first subdir)))))]
                           [path-parts (nth subdir 2)]))
                       (iterate-subdir dir))

        subdir-map (loop [files subdir-files res {}]
                     (if (seq files)
                       (let [[subdir-path file-set] (first files)
                             tree-path (take (* 2 (count subdir-path)) (interleave (mapv (memfn toString) subdir-path) (repeat :dir)))
                             files-in-dir (into {} (map (fn [filename] [filename {:name filename}]) file-set))
                             ]
                         (recur (rest files) (assoc-in (assoc-in res tree-path files-in-dir)
                                                       (conj (vec (butlast tree-path)) :name) (.toString (last subdir-path)))))
                       res
                       ))
        ]
    (assoc (get subdir-map ".") :name root-node-name)))

(defmulti file-to-model (fn [cascade-key orig-key project ^Reader reader]
                          cascade-key))

(defmethod file-to-model :default [cascade-key orig-key project ^Reader reader]
  (file-to-model (vec (butlast cascade-key)) orig-key project reader))

(defmethod file-to-model [] [cascade-key orig-key project ^Reader reader]
  {})

(defmethod file-to-model [core/settings-keyname "project.edn"] [cascade-key orig-key project ^Reader reader]
  (let [data (edn/read (java.io.PushbackReader. reader))]
    {:data (update data :working-dir (fn [workdir]
                                       (io/file workdir)))}
    ))

(defmethod file-to-model [core/settings-keyname] [cascade-key orig-key project ^Reader reader]
  (let [data (edn/read (java.io.PushbackReader. reader))]
    {:data data}
    ))

(defmethod file-to-model [core/relations-keyname] [cascade-key orig-key project ^Reader reader]
  (let [parser-context (prolog/parser-context nil)
        asts (doall (prolog/prolog-sentence-seq parser-context reader))
        [_ name arity]  (re-find #"(.*)_(.*)\.pl"  (last orig-key))]
    {:rel [name (Integer/valueOf ^String arity)]
     :ast asts}
    ))

(defn text-file-to-model [^Project p name-key]
  {:text (delay
           (slurp (name-keys-to-file p name-key)))})

(defmethod file-to-model [core/additions-keyname] [cascade-key orig-key project ^Reader reader]
  (text-file-to-model project orig-key))

(defmethod file-to-model [core/workdir-keyname] [cascade-key orig-key project ^Reader reader]
  (text-file-to-model project orig-key))

(defmethod file-to-model [core/output-keyname] [cascade-key orig-key project ^Reader reader]
  (text-file-to-model project orig-key))

(defn load-model-from-file [project name-key]
  (let [file-entry (get-in project (apply dir-keys name-key))
        ^File file (name-keys-to-file project name-key)]
    (with-open [reader (io/reader file)]
      (merge file-entry
             (file-to-model name-key name-key project reader))
      )))

#_(defn save-file [project name-key]
    (let [file-entry (get-in project (apply dir-keys name-key))
          ^File file (name-keys-to-file project name-key)]
      (with-open [writer (io/writer file)]

        )))

(defn zipiter-to-name-keys [zipiter]
  (into (list (:name (zip/node zipiter))) (reverse (map :name (zip/path zipiter)))))

(defn- load-file-into-iter-node [^Project p zipiter base-name-keys]
  (zip/replace zipiter
               (load-model-from-file p (zipiter-to-name-keys zipiter))))

(defn load-files [^Project p name-keys]
  (let [keys (apply dir-keys name-keys)]
    (assoc-in p keys
              (zip/root (loop [zipiter (file-model-zipper (get-in p keys))]
                          (if (zip/end? zipiter)
                            zipiter
                            (let [node (zip/node zipiter)]
                              (if (:dir node)
                                (recur (zip/next zipiter))
                                (recur (zip/next (load-file-into-iter-node p zipiter name-keys)))))))))))

(defn- load-plugin [^App app]
  {:pre [(not (nil? (:project app)))]}
  (let [plugin-key (:active-plugin (get-project-settings (:project app)))
        plugin-map (:plugins app)
        app (assoc-in app [:project :plugin] ((get plugin-map plugin-key (fn [] nil))))]
    app))

(defn load-settings [^App app]
  (let [p (:project app)
        proj-with-settings (assoc-in p (dir-keys settings-keyname) (generate-model-map (get-settings-dir p) settings-keyname))
        proj-with-settings (load-files proj-with-settings [settings-keyname])
        ]
    (-> app
        (assoc :project proj-with-settings)
        (load-plugin))))

(defn load-model-files [model-key ^Project p]
  (let [dir (name-keys-to-file p [model-key])]
    (fs/mkdir dir)
    (-> p
        (assoc-in (dir-keys model-key)
                  (generate-model-map dir model-key))
        (load-files [model-key]))))

(def load-output (partial load-model-files output-keyname))

(def load-working-dir (partial load-model-files workdir-keyname))

(def load-additions (partial load-model-files additions-keyname))

(def load-relations (partial load-model-files relations-keyname))

(defn load-base-project [^File dir]
  (->
    (base-project dir)
    load-additions
    load-output
    load-working-dir
    load-relations))

(defn load-project [^App app ^File dir]
  (let [app-with-project (load-settings (assoc app :project (base-project dir)))]
    (assoc app-with-project :project
                            (-> app-with-project
                                (:project)
                                (load-additions)
                                (load-relations)
                                (load-working-dir)
                                (load-output)))))

(defn init-app []
  (register-plugins (->App nil {}))
  )

(defn build-working-dir [^Project p]
  (rebuild-working-dir (:plugin p) p)
  )

(defn run-learning [^Project p]
  (run (:plugin p) p)
  )
