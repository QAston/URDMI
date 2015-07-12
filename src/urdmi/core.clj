(ns urdmi.core
  (:require [fx-clj.core :as fx])
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.zip :as zip]
            [clojure.string :as string]))

(defn load-fxml [filename]
  (let [loader (new javafx.fxml.FXMLLoader)]
    (.setLocation loader (io/resource ""))
    (.load loader (-> filename io/resource io/input-stream))))

(defn create-view []
  (let [click-ch (chan)
        btn (fx/button :#my-btn {:on-action click-ch        ;; You can bind a core.async channel directly to an event
                                 :text      "Next"})

        txt (fx/text "Initial text")
        view (fx/v-box txt btn)]

    (go
      (<! click-ch)
      (fx/run<! (fx/pset! txt "Next text"))
      (<! click-ch)
      (fx/run<!
        (fx/pset! txt "Last text")
        (fx/pset! btn {:text "Done"}))
      (println "Done listening to clicks"))

    view))

(def main-view (load-fxml "main.fxml"))
(defn asdf []
  main-view)

;(fx/sandbox #'asdf)

(import 'java.io.File)

(defn merge-addition
  "appends addition file to the working directory file, creates files if not present"
  [^File working-dir ^File additions-dir ^File addition-file-rel]
  (fs/mkdirs working-dir)
  (let [^Path target-file (io/file working-dir addition-file-rel)]
    (with-open [o (io/output-stream target-file :append true)]
      (java.nio.file.Files/copy (io/file additions-dir addition-file-rel) o))))



(defprotocol View
  (main-widget [this])
  (update-widget [this project data])
  (read-data [this]))

(defprotocol Plugin
  "All urdmi plugins must implement this protocol"
  (new-project-creation-view ^View [this app-event-in-channel] "returns a view for creating a ")
  (run [this project] "Run datamining engine associated to this plugin. Updates outout menu entries.")
  (update-working-dir [this project changed-entry] "Updates working dir files for given entry change")
  (new-entry-view ^View [this project entry to-app-channel] "Returns a view for editing/display of a menu entry"))

(defn register-plugin [])


(defrecord Project [relations, working-dir, additions, output, settings, project-dir])

(def project-keyname :project)
(def relations-keyname :relations)
(def workdir-keyname :working-dir)
(def output-keyname :output)
(def settings-keyname :settings)
(def additions-keyname :additions)

(def additions-dir-name "additions")
(def settings-dir-name "settings")
(def output-dir-name "output")
(def relations-dir-name "relations")
(def working-dir-default-folder "working_dir")

(defn base-project [project-dir]
  (->Project {} {} {} {} {"project.edn" {:data {:working-dir (fs/file project-dir working-dir-default-folder)}}} project-dir))

(defn get-working-dir [project]
  (:working-dir (:data (get (:settings project) "project.edn"))))

(defn get-additions-dir [^Project p]
  (fs/file (:project-dir p) additions-dir-name))

(defn get-relations-dir [^Project p]
  (fs/file (:project-dir p) relations-dir-name))

(defn get-settings-dir [^Project p]
  (fs/file (:project-dir p) settings-dir-name))

(defn get-output-dir [^Project p]
  (fs/file (:project-dir p) output-dir-name))

(defn save-entry! [^Project p entry]
  )

(defn load-entry! [^Project p entry]
  )

(defn load-relations [^Project p]
  (let [toread (fs/glob (get-relations-dir p) "*.pl")]
    (assoc p relations-keyname
             (into {} (map (fn [file] (let [name (string/join (fs/base-name file ".pl"))
                                            filename (.getName file)]
                                        [name {:filename filename
                                               :name     name}]))
                           toread)))))

(import 'java.nio.file.Path)
(defn relativize-path [^File src ^File target]
  (let [^Path src-path (.toPath src)
        ^Path target-path (.toPath target)]
    (.toFile (.relativize src-path target-path))))

(defn iterate-subdir [subdir]
  (map #(clojure.core/update % 0 (fn [dirname]
                                   (io/file "." (relativize-path subdir dirname)))) (fs/iterate-dir subdir)))

(defn generate-model-map [dir]
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

    (:dir (get subdir-map "."))))

(defn load-working-dir [^Project p]
  (assoc p workdir-keyname (generate-model-map (get-working-dir p))))

(defn load-additions [^Project p]
  (assoc p additions-keyname (generate-model-map (get-additions-dir p))))

(defn load-settings [^Project p]
  (assoc p settings-keyname (generate-model-map (get-settings-dir p))))

(defn load-output [^Project p]
  (assoc p output-keyname (generate-model-map (get-output-dir p))))

(defn load-project [^File dir]
  (-> (base-project dir)
       (load-settings)
       (load-additions)
       (load-relations)
       (load-working-dir)
       (load-output)))

(def entries-to-displaynames {
                              [project-keyname]                   "Project"
                              [project-keyname relations-keyname] "Relations"
                              [project-keyname workdir-keyname]   "Working Dir"
                              [output-keyname workdir-keyname]    "Output"
                              [settings-keyname workdir-keyname]  "Settings"
                              [additions-keyname workdir-keyname] "Additions"
                              })

(defn- subdir-map-to-vec [subdir-entry]
  (let [dirname (first subdir-entry)
        files (second subdir-entry)
        ]
    (vec (cons dirname
               (for [file files]
                 (subdir-map-to-vec file))))))

(defn model-map-to-menu-entries[m]
  )

(defn generate-menu-entries [^Project p]
  (let [relations (vec (cons relations-keyname (sort (map #(:name) (:relations p)))))
        working-dir (vec (cons workdir-keyname (model-map-to-menu-entries (get-working-dir p))))
        outputs (vec (cons output-keyname (sort (map #(:name) (:output p)))))
        additions (vec (cons additions-keyname (sort (map #(:name) (:additions p)))))
        settings (vec (cons settings-keyname (sort (map #(:name) (:settings p)))))
        ]
    [project-keyname relations working-dir outputs additions settings]))

