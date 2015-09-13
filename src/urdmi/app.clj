(ns urdmi.app
  "stuff depending both on core and plugins namespace
  mainly plugin loading and app init."
  (:use urdmi.core
        clojure.core.incubator)
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
           (java.io File Reader Writer)
           (java.util Date)))

;plugins should be loaded from a ./settings/plugins dir
(defn register-plugins [^App app]
  (-> app
      (register-plugin :ace #'ace/create)
      (register-plugin :aleph #'aleph/create)))

(defmulti file-to-model (fn [cascade-key orig-key ^App app ^Reader reader]
                          cascade-key))

(defmethod file-to-model :default [cascade-key orig-key ^App app ^Reader reader]
  (file-to-model (vec (butlast cascade-key)) orig-key app reader))

(defmethod file-to-model [] [cascade-key orig-key ^App app ^Reader reader]
  {})

(defmethod file-to-model [core/settings-keyname "project.edn"] [cascade-key orig-key ^App app ^Reader reader]
  (let [data (edn/read (java.io.PushbackReader. reader))]
    {:data (update data :working-dir (fn [workdir]
                                       (io/file workdir)))}
    ))

(defmethod file-to-model [core/settings-keyname] [cascade-key orig-key ^App app ^Reader reader]
  (let [data (edn/read (java.io.PushbackReader. reader))]
    {:data data}
    ))

(defmethod file-to-model [core/relations-keyname] [cascade-key orig-key ^App app ^Reader reader]
  (let [parser-context (prolog/parser-context nil)
        asts (doall (prolog/prolog-sentence-seq parser-context reader))
        [_ name arity] (re-find #"(.*)_(.*)\.pl" (last orig-key))]
    {:rel [name (Integer/valueOf ^String arity)]
     :ast asts}
    ))

(defn text-file-to-model [^App app name-key]
  {:text (delay
           (slurp (name-keys-to-file (:project app) name-key)))})

(defmethod file-to-model [core/additions-keyname] [cascade-key orig-key ^App app ^Reader reader]
  (text-file-to-model app orig-key))

(defmethod file-to-model [core/workdir-keyname] [cascade-key orig-key ^App app ^Reader reader]
  (text-file-to-model app orig-key))

(defmethod file-to-model [core/output-keyname] [cascade-key orig-key ^App app ^Reader reader]
  (text-file-to-model app orig-key))

(defn mark-file-read-synced [^App app file-key]
  (-> app
      (assoc-in [:fs-sync file-key] [:read (Date.)])))

(defn mark-file-write-synced [^App app file-key]
  (-> app
      (assoc-in [:fs-sync file-key] [:write (Date.)])))

(defn update-fs-sync-status [^App app file-key ^Date event-date]
  (let [[sync-type ^Date sync-date :as sync-status] (get-in
                                                      app
                                                      [:fs-sync file-key]
                                                      [:read (Date. 0)])

        needs-sync (and (not= sync-type :write)
                        (> (.getTime event-date) (.getTime sync-date)))]
    {:needs-sync needs-sync
     ;update after write-sync is allowed on next after first matching attempt
     ;change to read-sync, so that next sync occurs properly
     :app        (if (and (= sync-type :write)
                          (> (.getTime event-date) (.getTime sync-date)))
                   (assoc-in app [:fs-sync file-key] [:read sync-date])
                   app)}))

(defn load-file-to-model [^App app ^File file]
  (let [file-key (file-to-name-keys (:project app) file)

        app (if-not (get-in (:project app) (apply dir-keys (butlast file-key)))
              ; parent dir not found - load
              (load-file-to-model app (fs/parent file))
              app
              )
        data (get-in (:project app) (apply dir-keys file-key) {:name (fs/base-name file)})
        new-data (if (fs/directory? file)
                   (merge data {:dir {}})
                   (merge data
                          (with-open [reader (io/reader file)]
                            (file-to-model file-key file-key app reader)
                            )))

        project (assoc-in (:project app) (apply dir-keys file-key) new-data)]

    (->
      app
      (assoc :project project)
      (mark-file-read-synced file-key))))

(defn delete-file-from-model [^App app ^File file]
  (let [file-key (file-to-name-keys (:project app) file)]
    (-> app
        (dissoc-in [:fs-sync file-key])
        (dissoc-in (cons :project (seq (apply dir-keys file-key)))))))

(defmulti model-to-file (fn [cascade-key orig-key ^App app ^Writer writer]
                          cascade-key))

(defmethod model-to-file :default [cascade-key orig-key ^App app ^Writer writer]
  (model-to-file (vec (butlast cascade-key)) orig-key app writer))

(defmethod model-to-file [] [cascade-key orig-key ^App app ^Writer writer])

(defmethod model-to-file [core/settings-keyname "project.edn"] [cascade-key orig-key ^App app ^Writer writer]
  (let [data (-> app
                 (:project)
                 (get-in (apply dir-keys orig-key))
                 (:data)
                 (update :working-dir #(.toString %)))]
    (binding [*out* writer]
      (prn data))))

(defmethod model-to-file [core/settings-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (let [data (:data (get-in (:project app) (apply dir-keys orig-key)))]
    (binding [*out* writer]
      (pr data))))

(defmethod model-to-file [core/relations-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (let [parser-context (prolog/parser-context nil)
        ast (:ast (get-in (:project app) (apply dir-keys orig-key)))]
    (prolog/pretty-print-sentences parser-context ast writer)))

(defn text-model-to-file [^App app name-key ^Writer writer]
  (let [text @(:text (get-in (:project app) (apply dir-keys name-key)))]
    (binding [*out* writer]
      (print text))))

(defmethod model-to-file [core/additions-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (text-model-to-file app orig-key writer))

(defmethod model-to-file [core/workdir-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (text-model-to-file app orig-key writer))

(defmethod model-to-file [core/output-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (text-model-to-file app orig-key writer))

(defn save-model-to-file [^App app name-key]
  (let [^File file (name-keys-to-file (:project app) name-key)]
    (fs/mkdirs (fs/parent file))
    (with-open [writer (io/writer file)]
      (model-to-file name-key name-key app writer)
      )
    (mark-file-write-synced app name-key)))

(defn zipiter-to-name-keys [zipiter]
  (vec (into (list (:name (zip/node zipiter))) (reverse (map :name (zip/path zipiter))))))

(defn load-files [^App app dir-name-key]
  (let [project (:project app)
        root-dir (name-keys-to-file project dir-name-key)
        dirs (fs/iterate-dir root-dir)
        ]
    (loop [dirs dirs app app]
      (if-not (seq dirs)
        app
        (let [[root subdirs files :as dir] (first dirs)
              app (reduce (fn [app fname]
                            (load-file-to-model app (fs/file root fname))) app (concat files subdirs))
              ]
          (recur (rest dirs) app))
        ))))

(defn get-model-file-keys
  ([^Project p with-dirs]
   (apply concat (for [base-key [additions-keyname settings-keyname output-keyname workdir-keyname relations-keyname]]
                   (loop [zipiter (file-model-zipper (get-in p (dir-keys base-key))) ret []]
                     (if (zip/end? zipiter)
                       ret
                       (let [node (zip/node zipiter)]
                         (if (:dir node)
                           (recur (zip/next zipiter) (if with-dirs
                                                       (conj ret (zipiter-to-name-keys zipiter))
                                                       ret))
                           (recur (zip/next zipiter) (conj ret (zipiter-to-name-keys zipiter))))))))))
  ([^Project p]
   (get-model-file-keys p false)))

(defn get-model-dirs [^Project p]
  (vec (for [k [additions-keyname settings-keyname output-keyname workdir-keyname relations-keyname]]
         (core/name-keys-to-file p [k]))))

(defn save-files [^App app file-name-keys]
  (loop [app app file-name-keys file-name-keys]

    (if (seq file-name-keys)
      (recur (save-model-to-file app (first file-name-keys))
             (rest file-name-keys))
      app)))

(defn- load-plugin [^App app]
  {:pre [(not (nil? (:project app)))]}
  (let [plugin-key (:active-plugin (get-project-settings (:project app)))
        plugin-map (:plugins app)
        app (assoc-in app [:project :plugin] ((get plugin-map plugin-key (fn [] nil))))]
    app))

(defn load-settings [^App app]
  (-> app
      (load-files [settings-keyname])
      (load-plugin)))

(defn load-model-files [model-key ^App app]
  (let [p (:project app)
        dir (name-keys-to-file p [model-key])]
    (fs/mkdirs dir)
    (-> app
        (assoc-in [:project :dir model-key] {:name model-key :dir {}})
        (load-files [model-key]))))

(def load-output (partial load-model-files output-keyname))

(def load-working-dir (partial load-model-files workdir-keyname))

(def load-additions (partial load-model-files additions-keyname))

(def load-relations (partial load-model-files relations-keyname))

(defn load-project [^App app ^File dir]
  (->
    (load-settings (assoc app :project (base-project dir)))
    (load-additions)
    (load-relations)
    (load-working-dir)
    (load-output)))

(defn init-app []
  (register-plugins (->
                      (->App nil {})
                      (assoc :fs-sync {})))
  )

(defn build-working-dir [^Project p]
  (rebuild-working-dir (:plugin p) p)
  )

(defn run-learning [^Project p]
  (run (:plugin p) p)
  )
