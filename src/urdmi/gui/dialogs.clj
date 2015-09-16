(ns urdmi.gui.dialogs
  (:require [fx-clj.core :as fx])
  (:import (java.io File)
           (javafx.stage DirectoryChooser)
           (javafx.scene.control Alert Alert$AlertType ButtonType)))

(defn open-project [stage ^File dir]
  (.showDialog
    (doto (DirectoryChooser.)
      (.setInitialDirectory dir)
      (.setTitle "Select project dir")
      )
    stage))


(defn reload-modified-file [stage ^File file]
  (= ButtonType/YES
     (.orElse (.showAndWait (doto (Alert. Alert$AlertType/CONFIRMATION)
                      (.setTitle "File was changed")
                      (.. getDialogPane
                          (setContentText (str "File " file " was changed outside of the application. Do you wish to reload it?")))
                      (.. getButtonTypes
                          (clear))
                      (.. getButtonTypes
                          (add ButtonType/YES)
                          )
                      (.. getButtonTypes
                          (add ButtonType/NO)
                          ))
                    ) ButtonType/NO)))