(ns urdmi.gui.dialogs
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [urdmi.prolog :as prolog])
  (:import (java.io File)
           (javafx.stage DirectoryChooser)
           (javafx.scene.control Alert Alert$AlertType ButtonType TextInputDialog Dialog TextField)
           (java.util Optional)
           (javafx.geometry Pos)
           (javafx.scene.text TextAlignment)
           (org.controlsfx.validation.decoration StyleClassValidationDecoration)
           (javafx.util Callback)))

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
                                                 (setAll [ButtonType/YES ButtonType/NO])
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

(defn new-relation [stage parser-context]
  (.orElse
    (.showAndWait
     (let [validation (gui/validation-support (StyleClassValidationDecoration.))
           name-field ^TextField (fx/text-field {:prompt-text "Name"})
           arity-field ^TextField (fx/text-field {:prompt-text "Arity" :max-width 40})
           validate-arity-fn (fn [s]
                               (try
                                 (>= (Long/parseLong ^String s) 0)
                                 (catch NumberFormatException e
                                   false)))
           validate-name-fn (fn [s] (prolog/parse-single-atom parser-context s))
           dialog (doto (Dialog.)
                    (.setTitle "New relation")
                    (.. getDialogPane (setContent (fx/h-box {}
                                                            name-field
                                                            arity-field
                                                            )))
                    (.. getDialogPane getButtonTypes (setAll [ButtonType/OK, ButtonType/CANCEL]))
                    (.setResultConverter (reify Callback
                                           (call [this param]
                                             (if (= param ButtonType/OK)
                                               [(.getText name-field) (Long/parseLong ^String (.getText arity-field))]
                                               nil
                                               ))))
                    )
           ok-button (.. dialog getDialogPane (lookupButton ButtonType/OK))
           update-button (fn [obs old new]
                           (.setDisable ok-button (not (and
                                                         (validate-name-fn (.getText name-field))
                                                         (validate-arity-fn (.getText arity-field)))))
                           )]
       (.setDisable ok-button true)
       (gui/validate-control validation arity-field validate-arity-fn "Arity must be a number")
       (gui/validate-control validation name-field validate-name-fn "Name must be a valid prolog predicate")
       (gui/on-changed (.textProperty name-field) update-button)
       (gui/on-changed (.textProperty arity-field) update-button)
       dialog
       ))
    nil))