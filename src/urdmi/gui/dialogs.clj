(ns urdmi.gui.dialogs
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [urdmi.prolog :as prolog]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [urdmi.importer :as importer]
            [urdmi.core :as core])
  (:import (java.io File)
           (javafx.stage DirectoryChooser FileChooser)
           (javafx.scene.control Alert Alert$AlertType ButtonType TextInputDialog Dialog TextField ChoiceBox)
           (javafx.geometry Pos HPos)
           (org.controlsfx.validation.decoration StyleClassValidationDecoration)
           (javafx.util Callback StringConverter)
           (javafx.scene.layout Priority HBox GridPane ColumnConstraints)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty)
           (org.controlsfx.control ListSelectionView PropertySheet PropertySheet$Mode)
           (javafx.beans.binding StringExpression)
           (javafx.scene.control.cell TextFieldListCell)
           (javafx.collections ObservableList)
           (java.util Collection)))

(defn open-project [stage ^File dir]
  (.showDialog
    (doto (DirectoryChooser.)
      (.setInitialDirectory dir)
      (.setTitle "Select project dir")
      )
    stage))

(defn error-alert [stage title text]
  (.showAndWait
    (doto (Alert. Alert$AlertType/ERROR)
      (.setTitle title)
      (.setContentText text))))

(defn yes-no-dialog [stage content-text title-text]
  (= ButtonType/YES (.orElse (.showAndWait (doto (Alert. Alert$AlertType/CONFIRMATION)
                                             (.setTitle title-text)
                                             (.. getDialogPane
                                                 (setContentText content-text))
                                             (.. getButtonTypes
                                                 (setAll [ButtonType/YES ButtonType/NO])
                                                 ))
                                           ) ButtonType/NO)))

(def button-to-kw {ButtonType/YES :yes ButtonType/NO :no ButtonType/CANCEL :cancel})

(defn yes-no-cancel-dialog [stage content-text title-text]
  (button-to-kw (.orElse (.showAndWait (doto (Alert. Alert$AlertType/CONFIRMATION)
                                         (.setTitle title-text)
                                         (.. getDialogPane
                                             (setContentText content-text))
                                         (.. getButtonTypes
                                             (setAll [ButtonType/YES ButtonType/NO ButtonType/CANCEL])
                                             ))
                                       ) ButtonType/CANCEL)))

(defn save-modified-on-exit [stage]
  (yes-no-cancel-dialog stage "Save modified files on exit" "Some files are modified, but not saved. Do you wish to save them?"))


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
      (let [validation (gui/validation-support)
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
        (gui/validate-control validation arity-field validate-arity-fn "Arity must be a number > 0")
        (gui/validate-control validation name-field validate-name-fn "Name must be a valid prolog atom")
        (gui/on-changed (.textProperty name-field) update-button)
        (gui/on-changed (.textProperty arity-field) update-button)
        (gui/default-stylesheet (.getDialogPane dialog))
        dialog
        ))
    nil))

(def key-type-to-string {:primary "Primary Key"
                         :foreign "Foreign Key"
                         :none "(None)"})

(defn- relations-column-description-widget [prop validation validate-fn]
  (let [name-prop (SimpleStringProperty. (:name (.getValue prop)))
        key-prop (SimpleObjectProperty. (:key (.getValue prop)))
        text-field (fx/text-field {:text (.getValue name-prop)})]

    (gui/on-changed name-prop
                    (fn [obs old new]
                      (.setValue prop {:name new :key (:key (.getValue prop))})))
    (gui/on-changed key-prop
                    (fn [obs old new]
                      (.setValue prop {:name (:name (.getValue prop)) :key new})))
    (gui/loose-bind (.textProperty text-field) name-prop)
    (gui/validate-control validation text-field validate-fn
                          "Column name must be a valid prolog atom.")
    (fx/h-box {}
              (doto (gui/choice-box (gui/observable-list (keys key-type-to-string)) key-prop)
                (.setConverter (proxy
                                 [StringConverter] []
                                 (toString [obj]
                                   (key-type-to-string obj)
                                   ))))
              text-field)
    ))

(defn relations-column-editor [column-definitions parser-context]
  (let [
        validation (gui/validation-support)
        validate-fn (fn [t]
                      (boolean (prolog/parse-single-atom parser-context t)))
        column-descriptions-widgets (vec (for [[i definition] (map-indexed vector column-definitions)]
                                           (let [prop (SimpleObjectProperty. definition)]
                                             (gui/->PropertyItemEditor
                                               (relations-column-description-widget prop validation validate-fn) (str "Term" i) prop))))

        editor (doto (PropertySheet. (gui/observable-list column-descriptions-widgets))
                 (.setModeSwitcherVisible false)
                 (.setSearchBoxVisible false)
                 (.setMode PropertySheet$Mode/NAME)
                 (.setPropertyEditorFactory gui/property-editor-factory)
                 )

        dialog (doto (Dialog.)
                 (.setTitle "Edit relation columns")
                 (.. getDialogPane (setContent editor))
                 (.setWidth 400.0)
                 (.setHeight 600.0)
                 (.. getDialogPane getButtonTypes (setAll [ButtonType/OK, ButtonType/CANCEL]))
                 (.setResultConverter (reify Callback
                                        (call [this param]
                                          (if (= param ButtonType/OK)
                                            (mapv (memfn getValue) column-descriptions-widgets)
                                            nil
                                            )))))

        ok-button (.. dialog getDialogPane (lookupButton ButtonType/OK))
        update-button (fn [obs old new]
                        (.setDisable ok-button
                                     (->>
                                       (mapv (memfn getValue) column-descriptions-widgets)
                                       (mapv (fn [desc]
                                               (validate-fn (:name desc))))
                                       (every? boolean)
                                       (not)))
                        )]
    (doseq [prop column-descriptions-widgets]
      (gui/on-changed (.obj-property prop) update-button))
    (.setDisable ok-button true)
    (gui/default-stylesheet (.getDialogPane dialog))
    (.orElse (.showAndWait dialog) nil)))

(defn- make-plugin-selection-widget [plugins-list]
  (doto (ChoiceBox.)
    (.setConverter (proxy [StringConverter] []
                     (fromString [s]
                       (keyword s))
                     (toString [v]
                       (name v))))
    (.. getItems (setAll plugins-list))
    (.setValue (first plugins-list))))

(defn new-project [stage plugins-list location-validation-fn]
  (.orElse
    (.showAndWait
      (let [validation (gui/validation-support)
            plugin-widget (make-plugin-selection-widget plugins-list)

            location-property (SimpleStringProperty. "")

            location-widget (gui/make-absolute-directory-select-widget
                              (fs/file ".")
                              location-property
                              "Select project directory"
                              validation
                              location-validation-fn
                              "Path must be absolute and point to a directory, directory must not have an existing project in it.")

            grid (doto (GridPane.)
                   (.setAlignment Pos/CENTER)
                   (.setHgap 10.0)
                   (.setVgap 12.0)
                   (.. getColumnConstraints
                       (setAll [(doto (ColumnConstraints.)
                                  (.setHalignment HPos/LEFT))
                                (doto (ColumnConstraints.)
                                  (.setHalignment HPos/RIGHT))
                                ]))
                   (.add (fx/label {:text "Plugin"}) 0 0)
                   (.add plugin-widget 1 0)
                   (.add (fx/label {:text "Project Location"}) 0 1)
                   (.add location-widget 1 1)
                   )

            dialog (doto (Dialog.)
                     (.setTitle "New project")
                     (.. getDialogPane (setContent grid))
                     (.. getDialogPane getButtonTypes (setAll [ButtonType/OK, ButtonType/CANCEL]))
                     (.setResultConverter (reify Callback
                                            (call [this param]
                                              (if (= param ButtonType/OK)
                                                {:plugin (.getValue plugin-widget) :project-dir (io/file (.getValue location-property))}
                                                nil
                                                )))))
            ok-button (.. dialog getDialogPane (lookupButton ButtonType/OK))
            update-button (fn [obs old new]
                            (.setDisable ok-button (not (location-validation-fn (.getValue location-property)))))
            ]
        (.setDisable ok-button true)
        (gui/on-changed location-property update-button)
        (gui/default-stylesheet (.getDialogPane dialog))
        dialog
        ))
    nil))

(defn import-relation
  "Returns relation->ast map or nil"
  [stage parser-context init-dir]
  (let [
        available-relations (gui/observable-list)
        to-import-relations ^ObservableList (gui/observable-list)
        selected-file (SimpleObjectProperty.)
        select-file-label-text (doto (SimpleStringProperty.)
                                 (.setValue "Import from: ")
                                 (.concat selected-file))
        list-selection-view
        (doto (ListSelectionView.)
          (.setSourceHeader (fx/label {:text "Relations to skip"}))
          (.setTargetHeader (fx/label {:text "Relations to import"}))
          (.setSourceItems available-relations)
          (.setTargetItems to-import-relations)
          (.setCellFactory (reify Callback
                             (call [this list-view]
                               (TextFieldListCell. gui/relation-string-converter)))))

        file-click-action (fn [e]
                            (.setValue selected-file
                                       (.showOpenDialog (doto (FileChooser.)
                                                          (.setTitle "Select file to import")
                                                          (.setInitialDirectory init-dir))
                                                        stage))
                            )

        dir-click-action (fn [e]
                           (.setValue selected-file
                                      (.showDialog (doto (DirectoryChooser.)
                                                     (.setTitle "Select directory to import")
                                                     (.setInitialDirectory init-dir))
                                                   stage))
                           )

        result (SimpleObjectProperty.)

        grid (doto (GridPane.)
               (.setAlignment Pos/CENTER)
               (.setHgap 10.0)
               (.setVgap 12.0)
               (.. getColumnConstraints
                   (setAll [(doto (ColumnConstraints.)
                              (.setHalignment HPos/LEFT))
                            (doto (ColumnConstraints.)
                              (.setHalignment HPos/RIGHT))
                            ]))
               (.add (fx/label {:text select-file-label-text}) 0 0)
               (.add (fx/h-box {:spacing   0.0
                                :max-width 80.0}
                               (fx/button {:text      "File"
                                           :on-action file-click-action})
                               (fx/button {:text      "Dir"
                                           :on-action dir-click-action})) 1 0)
               (.add list-selection-view 0 1 2 1)
               )
        dialog (doto (Dialog.)
                 (.setTitle "Import relations from file")
                 (.. getDialogPane (setContent grid))
                 (.. getDialogPane getButtonTypes (setAll [ButtonType/OK, ButtonType/CANCEL]))
                 (.setResultConverter (reify Callback
                                        (call [this param]
                                          (if (= param ButtonType/OK)
                                            (select-keys (.getValue result) (vec to-import-relations))
                                            nil
                                            )))))]
    (gui/on-changed selected-file (fn [obs old new]
                                    (.setValue select-file-label-text (str "Import from: " new))
                                    (.clear available-relations)
                                    (.setValue result nil)
                                    (if new
                                      (when-let [rels (importer/load-from-location new parser-context)]
                                        (.setAll to-import-relations ^Collection (keys rels))
                                        (.setValue result rels)
                                        )
                                      (.clear to-import-relations)
                                      )))
    (.orElse (.showAndWait dialog) nil)))
