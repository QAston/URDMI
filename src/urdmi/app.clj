(ns urdmi.app
  "stuff depending both on core and plugins namespace
  mainly plugin loading and app init."
  (:use urdmi.core
        clojure.core.incubator)
  (:require [urdmi.plugin.ace.core :as ace]
            [urdmi.plugin.ace.gui :as ace-gui]
            [urdmi.plugin.aleph.core :as aleph]
            [urdmi.plugin.aleph.gui :as aleph-gui]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
            [urdmi.prolog :as prolog]
            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [urdmi.core :as core]
            [clojure.core.async :as async])
  (:import (urdmi.core App Project ModelDiff)
           (java.io File Reader Writer)
           (java.util Date)))

;plugins should be loaded from a ./settings/plugins dir
(defn register-plugins [^App app]
  (-> app
      (register-plugin :ace #'ace/create)
      (register-plugin :aleph #'aleph/create)))

(defn plugin-parser-context [^App app]
  (core/get-parser-context (:plugin (:project app))))

(defn plugin [^App app]
  (:plugin (:project app)))

(defmulti file-to-model (fn [cascade-key orig-key ^App app]
                          cascade-key))

(defmethod file-to-model :default [cascade-key orig-key ^App app]
  (file-to-model (vec (butlast cascade-key)) orig-key app))

(defmethod file-to-model [] [cascade-key orig-key ^App app]
  {})

(defmethod file-to-model [core/settings-keyname "project.edn"] [cascade-key orig-key ^App app]
  (->FileItem
    (delay
      (with-open [reader (io/reader (item-key-to-file (:project app) orig-key))]
        (->
          (edn/read (java.io.PushbackReader. reader))
          (update :working-dir (fn [workdir]
                                 (io/file workdir)))))))
  )

(defmethod file-to-model [core/settings-keyname] [cascade-key orig-key ^App app]
  (->FileItem
    (delay
      (with-open [reader (io/reader (item-key-to-file (:project app) orig-key))]
        (edn/read (java.io.PushbackReader. reader)))))
  )

(defmethod file-to-model [core/relations-keyname] [cascade-key orig-key ^App app]
  (let [parser-context (plugin-parser-context app)
        [_ name arity] (re-find #"(.*)_(.*)\.pl" (last orig-key))]
    (map->FileItem
      {:rel  [name (Integer/valueOf ^String arity)]
       :data (delay
               (with-open [reader (io/reader (item-key-to-file (:project app) orig-key))]
                 (doall (prolog/prolog-sentence-seq parser-context reader))))})
    ))

(defn text-file-to-model [^App app item-key]
  (->
    (->FileItem (delay
                 (slurp (item-key-to-file (:project app) item-key))))
    (assoc :text true)))

(defmethod file-to-model [core/additions-keyname] [cascade-key orig-key ^App app]
  (text-file-to-model app orig-key))

(defmethod file-to-model [core/workdir-keyname] [cascade-key orig-key ^App app]
  (text-file-to-model app orig-key))

(defmethod file-to-model [core/output-keyname] [cascade-key orig-key ^App app]
  (text-file-to-model app orig-key))

(defn mark-file-desynced [^App app item-key]
  (-> app
      (dissoc-in [:fs-sync item-key])))

(defn is-dir [app item-key]
  (:dir (get-in (:project app) (apply core/model-map-keys item-key))))

(defn is-desynced [^App app item-key]
  (and (not (is-dir app item-key))
       (not (get-in app [:fs-sync item-key]))))

(defn mark-file-read-synced [^App app item-key]
  (-> app
      (assoc-in [:fs-sync item-key] [:read (Date.)])))

(defn mark-file-write-synced [^App app item-key]
  (-> app
      (assoc-in [:fs-sync item-key] [:write (Date.)])))

(defn update-fs-sync-status [^App app item-key ^Date event-date]
  (let [[sync-type ^Date sync-date :as sync-status] (get-in
                                                      app
                                                      [:fs-sync item-key]
                                                      [:read (Date. 0)])

        needs-sync (and (not= sync-type :write)
                        (> (.getTime event-date) (.getTime sync-date)))]
    {:needs-sync needs-sync
     ;update after write-sync is allowed on next after first matching attempt
     ;change to read-sync, so that next sync occurs properly
     :app        (if (and (= sync-type :write)
                          (> (.getTime event-date) (.getTime sync-date)))
                   (assoc-in app [:fs-sync item-key] [:read sync-date])
                   app)}))

(defn load-file-to-model [^App app ^File file]
  (let [item-key (file-to-item-key (:project app) file)

        app (if-not (get-in (:project app) (apply model-map-keys (butlast item-key)))
              ; parent dir not found - load
              (load-file-to-model app (fs/parent file))
              app
              )

        new-data (if (fs/directory? file)
                   (->DirItem (get-in (:project app) (conj (apply model-map-keys item-key) :dir) {}))
                   (file-to-model item-key item-key app)
                   )

        project (assoc-in (:project app) (apply model-map-keys item-key) (assoc new-data :name (last item-key)))]

    (->
      app
      (assoc :project project)
      (mark-file-read-synced item-key))))

(defn delete-model-page [^App app item-key]
  (-> app
      (dissoc-in [:fs-sync item-key])
      (dissoc-in (cons :project (seq (apply model-map-keys item-key))))))

(defmulti model-to-file (fn [cascade-key orig-key ^App app ^Writer writer]
                          cascade-key))

(defmethod model-to-file :default [cascade-key orig-key ^App app ^Writer writer]
  (model-to-file (vec (butlast cascade-key)) orig-key app writer))

(defmethod model-to-file [] [cascade-key orig-key ^App app ^Writer writer])

(defmethod model-to-file [core/settings-keyname "project.edn"] [cascade-key orig-key ^App app ^Writer writer]
  (let [data (-> app
                 (:project)
                 (get-in (apply model-map-keys orig-key))
                 (:data)
                 (deref)
                 (update :working-dir #(.toString %)))]
    (binding [*out* writer]
      (prn data))))

(defmethod model-to-file [core/settings-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (let [data @(:data (get-in (:project app) (apply model-map-keys orig-key)))]
    (binding [*out* writer]
      (pr data))))

(defmethod model-to-file [core/relations-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (let [parser-context (plugin-parser-context app)
        ast @(:data (get-in (:project app) (apply model-map-keys orig-key)))]
    (prolog/pretty-print-sentences parser-context ast writer)))

(defn text-model-to-file [^App app item-key ^Writer writer]
  (let [text @(:data (get-in (:project app) (apply model-map-keys item-key)))]
    (binding [*out* writer]
      (print text))))

(defmethod model-to-file [core/additions-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (text-model-to-file app orig-key writer))

(defmethod model-to-file [core/workdir-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (text-model-to-file app orig-key writer))

(defmethod model-to-file [core/output-keyname] [cascade-key orig-key ^App app ^Writer writer]
  (text-model-to-file app orig-key writer))

(defn save-model-to-file [^App app item-key]
  (if (is-dir app item-key)
    (do
      (fs/mkdirs (item-key-to-file (:project app) item-key))
      app)
    (let [^File file (item-key-to-file (:project app) item-key)]
      (fs/mkdirs (fs/parent file))
      (with-open [writer (io/writer file)]
        (model-to-file item-key item-key app writer)
        )
      (mark-file-write-synced app item-key))))

(defn zipiter-to-item-key [zipiter]
  (vec (into (list (:name (zip/node zipiter))) (reverse (map :name (zip/path zipiter))))))

(defn load-files [^App app dir-item-key]
  (reduce load-file-to-model app (dir-seq (:project app) dir-item-key)))

(defn get-model-item-keys
  ([^Project p with-dirs]
   (apply concat (for [base-key [additions-keyname settings-keyname output-keyname workdir-keyname relations-keyname]]
                   (loop [zipiter (file-model-zipper (get-in p (model-map-keys base-key))) ret []]
                     (if (zip/end? zipiter)
                       ret
                       (let [node (zip/node zipiter)]
                         (if (:dir node)
                           (recur (zip/next zipiter) (if with-dirs
                                                       (conj ret (zipiter-to-item-key zipiter))
                                                       ret))
                           (recur (zip/next zipiter) (conj ret (zipiter-to-item-key zipiter))))))))))
  ([^Project p]
   (get-model-item-keys p false)))

(defn get-model-dirs [^Project p]
  (vec (for [k [additions-keyname settings-keyname output-keyname workdir-keyname relations-keyname]]
         (core/item-key-to-file p [k]))))

(defn save-files [^App app item-key]
  (loop [app app file-item-keys item-key]

    (if (seq file-item-keys)
      (recur (save-model-to-file app (first file-item-keys))
             (rest file-item-keys))
      app)))

(defn- instantiate-plugin [^App app]
  {:pre [(not (nil? (:project app)))]}
  (let [plugin-key (:active-plugin (get-project-settings (:project app)))
        plugin-map (:plugins app)
        app (assoc-in app [:project :plugin] ((get plugin-map plugin-key (fn [] nil))))]
    app))

(defn load-settings [^App app]
  (-> app
      (load-files [settings-keyname])
      (instantiate-plugin)))

(defn load-model-files [item-key ^App app]
  (let [p (:project app)
        dir (item-key-to-file p [item-key])]
    (fs/mkdirs dir)
    (-> app
        (assoc-in [:project :dir item-key] (map->DirItem {:name item-key :dir {}}))
        (load-files [item-key]))))

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

(defn new-project [app project-dir plugin]
  (-> app
      (assoc :fs-sync {})
      (assoc :project (->
                        (core/base-project project-dir)
                        (update-in (apply core/model-map-keys [:settings "project.edn"]) #(assoc % :data
                                                                                                   (instant (assoc @(:data %) :active-plugin plugin))))
                        (assoc-in (apply core/model-map-keys [core/output-keyname])
                                  (map->DirItem {:name core/output-keyname
                                                 :dir  {}}))
                        (assoc-in (apply core/model-map-keys [core/additions-keyname])
                                  (map->DirItem {:name core/additions-keyname
                                                 :dir  {}}))
                        (assoc-in (apply core/model-map-keys [core/relations-keyname])
                                  (map->DirItem {:name core/relations-keyname
                                                 :dir  {}}))
                        (assoc-in (apply core/model-map-keys [core/workdir-keyname])
                                  (map->DirItem {:name core/workdir-keyname
                                                 :dir  {}}))))

      (instantiate-plugin)))

(defn init-app []
  (register-plugins (->
                      (->App nil {})
                      (assoc :fs-sync {})
                      (assoc :ui-requests (async/chan))))
  )