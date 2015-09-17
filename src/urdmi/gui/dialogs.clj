(ns urdmi.gui.dialogs
  (:require [fx-clj.core :as fx])
  (:import (java.io File)
           (javafx.stage DirectoryChooser)
           (javafx.scene.control Alert Alert$AlertType ButtonType)
           (java.util Optional)))

(defn open-project [stage ^File dir]
  (.showDialog
    (doto (DirectoryChooser.)
      (.setInitialDirectory dir)
      (.setTitle "Select project dir")
      )
    stage))

(defn yes-no-dialog [stage content-text title-text]
  (= ButtonType/YES (.orElse (.showAndWait (doto (Alert. Alert$AlertType/CONFIRMATION)
                                             (.setTitle title-text)
                                             (.. getDialogPane
                                                 (setContentText content-text))
                                             (.. getButtonTypes
                                                 (clear))
                                             (.. getButtonTypes
                                                 (add ButtonType/YES)
                                                 )
                                             (.. getButtonTypes
                                                 (add ButtonType/NO)
                                                 ))
                                           ) ButtonType/NO)))


(defn reload-modified-file [stage ^File file]
  (let [title-text "File was changed"
        content-text (str "File " file " was changed outside of the application. Do you wish to reload it?")]
    (yes-no-dialog stage content-text title-text)))

(defn confirm-saving-all [stage operation]
  (let [title-text "Unsaved files"
        content-text (str "Some files in the project are modified but not saved. Do you wish to save the files and continue " operation " operation?")]
    (yes-no-dialog stage content-text title-text)))