(ns urdmi.core
  (:require [fx-clj.core :as fx])
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(defn load-fxml [filename]
  (let [loader (new javafx.fxml.FXMLLoader)]
    (.setLocation loader (io/resource ""))
    (.load loader (-> filename io/resource io/input-stream))))

(defn create-view []
  (let [click-ch (chan)
        btn (fx/button :#my-btn {:on-action click-ch ;; You can bind a core.async channel directly to an event
                                 :text "Next"})

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
(import 'java.nio.file.Path)
(defn merge-addition
  "appends addition file to the working directory file, creates files if not present"
  [^File working-dir ^File additions-dir ^File addition-file-rel]
  (fs/mkdirs working-dir)
  (let [^Path target-file (.toFile (.resolve (.toPath working-dir) (.toPath addition-file-rel)))]
    (with-open [o (io/output-stream target-file :append true)]
      (java.nio.file.Files/copy (.resolve (.toPath additions-dir) (.toPath addition-file-rel)) o))))

