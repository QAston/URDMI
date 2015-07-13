(ns urdmi.core
  (:require [fx-clj.core :as fx])
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.zip :as zip]
            [clojure.edn :as edn]
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


(defrecord Project [dir, project-dir])

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
  (->Project {settings-keyname {:name settings-keyname
                                :dir
                                {"project.edn" {:name "project.edn"
                                                :data {:working-dir (io/file working-dir-default-folder)}}}}} project-dir))

(defn dir-keys
  "constructs a vector of keys into project :dir map from given node names (name-keys)"
  [& name-keys]
  (vec (dedupe (take (* 2 (count name-keys)) (interleave (repeat :dir) name-keys)))))

(defn to-name-keys
  "constructs a vector of project node-names (model-keys) from a vector of keys into project :dir map"
  [dir-keys]
  (vec (remove (fn [k] (= :dir k)) dir-keys)))

(defn get-working-dir [project]
  (let [wdir (:working-dir (:data (get (:dir (:settings (:dir project))) "project.edn")))]
    (if (fs/absolute? wdir)
      wdir
      (io/file (:project-dir project) wdir))))

(defn get-additions-dir [^Project p]
  (fs/file (:project-dir p) additions-dir-name))

(defn get-relations-dir [^Project p]
  (fs/file (:project-dir p) relations-dir-name))

(defn get-settings-dir [^Project p]
  (fs/file (:project-dir p) settings-dir-name))

(defn get-output-dir [^Project p]
  (fs/file (:project-dir p) output-dir-name))

(def ^:private model-path-to-file-map
  {relations-keyname get-relations-dir
   workdir-keyname   get-working-dir
   output-keyname    get-output-dir
   settings-keyname  get-settings-dir
   additions-keyname get-additions-dir})

(defn name-keys-to-file
  "Resolve name-keys to a java.io.File"
  ^File [^Project p name-keys]
  (apply fs/file (keep identity (map (fn [element]
                                       (let [conv-fn (get model-path-to-file-map element)]
                                         (if conv-fn
                                           (conv-fn p)
                                           element))) name-keys))))

#_(defn file-to-name-keys
  [])

(defn file-model-branch? [m]
  (:dir m))

(defn file-model-children [m]
  (map second (:dir m)))

(defn file-model-make-node [node children]
  (assoc node :dir (into {} (map (fn [child]
                                   [(:name child) child]) children))))

(defn file-model-zipper [root]
  (zip/zipper file-model-branch? file-model-children file-model-make-node root))

(import 'java.nio.file.Path)
(defn relativize-path [^File src ^File target]
  (let [^Path src-path (.toPath src)
        ^Path target-path (.toPath target)]
    (.toFile (.relativize src-path target-path))))

(defn iterate-subdir [subdir]
  (map #(clojure.core/update % 0 (fn [dirname]
                                   (io/file "." (relativize-path subdir dirname)))) (fs/iterate-dir subdir)))

(defn read-edn-data [^File file]
  (with-open [i (io/reader file)]
    (edn/read (java.io.PushbackReader. i))))

(defn save-entry [^Project p name-keys]
  )

(defn load-entry [^Project p name-keys]
  )

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

(defn load-working-dir [^Project p]
  (assoc-in p (dir-keys workdir-keyname) (generate-model-map (get-working-dir p) workdir-keyname)))

(defn load-additions [^Project p]
  (assoc-in p (dir-keys additions-keyname) (generate-model-map (get-additions-dir p) additions-keyname)))

(defn zipiter-to-name-keys [zipiter]
  (into (list (:name (zip/node zipiter))) (map :name (zip/path zipiter))))

#_(defn- read-edn-data-into-iter-node [^Project p zipiter node]
  (zip/replace zipiter (assoc node :data (read-edn-data (name-keys-to-file p (zipiter-to-name-keys zipiter))))))

#_(defn load-files[^Project p]
  (assoc-in p (dir-keys settings-keyname :dir)
            (zip/root (loop [zipiter (file-model-zipper (get-in p (dir-keys settings-keyname)))]
                        (if (zip/end? zipiter)
                          zipiter
                          (let [node (zip/node zipiter)]
                            (if (:dir node)
                              (recur (zip/next zipiter))
                              (recur (zip/next (read-edn-data-into-iter-node p zipiter node))))
                            )
                          )
                        ))))

(defn deserialize-project-edn [data]
  (update data :working-dir (fn [workdir]
                              (io/file workdir))))

(defn serialize-project-edn [data]
  (update data :working-dir (fn [workdir-file]
                              (.toString workdir-file))))

(defn load-settings [^Project p]
  (let [proj-with-settings (assoc-in p (dir-keys settings-keyname) (generate-model-map (get-settings-dir p) settings-keyname))
        proj-settings-keys [settings-keyname "project.edn"]]
    (assoc-in proj-with-settings
              (conj (apply dir-keys proj-settings-keys) :data)
              (deserialize-project-edn (read-edn-data (name-keys-to-file proj-with-settings proj-settings-keys))))))

(defn load-output [^Project p]
  (assoc-in p (dir-keys output-keyname) (generate-model-map (get-output-dir p) output-keyname)))

(defn load-relations [^Project p]
  (let [toread (fs/glob (get-relations-dir p) "*.pl")]
    (assoc-in p (dir-keys relations-keyname)
              {:dir (into {} (map (fn [file] (let [name (string/join (fs/base-name file ".pl"))
                                                   filename (.getName file)]
                                               [filename {:name filename
                                                      :relname     name}]))
                                  toread))})))

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

(defn model-map-to-menu-entries [m]
  )

(defn generate-menu-entries [p]
  (let [relations (vec (cons relations-keyname (sort (map #(:name) (:relations p)))))
        working-dir (vec (cons workdir-keyname (model-map-to-menu-entries nil)))
        outputs (vec (cons output-keyname (sort (map #(:name) (:output p)))))
        additions (vec (cons additions-keyname (sort (map #(:name) (:additions p)))))
        settings (vec (cons settings-keyname (sort (map #(:name) (:settings p)))))
        ]
    [project-keyname relations working-dir outputs additions settings]))

