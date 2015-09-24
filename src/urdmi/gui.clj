(ns urdmi.gui
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [me.raynes.fs :as fs])
  (:import (javafx.collections ObservableList FXCollections)
           (java.util Collection)
           (javafx.beans.value ObservableValue ChangeListener WritableValue)
           (org.controlsfx.validation ValidationSupport Validator Severity)
           (org.controlsfx.validation.decoration ValidationDecoration)
           (javafx.scene.control Control TableCell Labeled TextField)
           (java.util.function Predicate)
           (clojure.lang IFn)
           (javafx.util Callback)
           (org.controlsfx.tools ValueExtractor)
           (javafx.scene.input KeyCodeCombination KeyCode)
           (javafx.event EventHandler)
           (javafx.beans.property Property)
           (java.util Optional)
           (javafx.scene.control ToggleButton)
           (javafx.geometry Insets)
           (java.io File)
           (javafx.scene.layout HBox Priority)
           (javafx.stage DirectoryChooser)
           (org.controlsfx.validation ValidationSupport)
           (javafx.beans.property SimpleStringProperty)
           (org.controlsfx.validation.decoration StyleClassValidationDecoration)
           (org.controlsfx.control SegmentedButton PropertySheet$Item)
           (org.controlsfx.property.editor PropertyEditor DefaultPropertyEditorFactory)))

(defn load-fxml [filename]
  (let [loader (new javafx.fxml.FXMLLoader (io/resource filename))]
    (.load loader)))

;application level abstraction, implements presenting data from app model
(defprotocol ContentPage
  (container-node [this])
  (show-data [this data data-key])
  (read-data [this]))

;implementation level abstraction, gui taking viewmodel
(defprotocol DataWidget
  (get-node [this])
  (set-data! [this data data-key])
  (get-data [this]))

(defprotocol PluginGui
  (new-project-creation-view ^ContentPage [this app-event-in-channel] "returns a view for creating a ")
  (new-entry-view ^ContentPage [this project entry to-app-channel] "Returns a view for editing/display of a menu entry"))

(ValueExtractor/addObservableValueExtractor (reify Predicate
                                              (test [this control]
                                                (instance? Labeled control)))
                                            (reify Callback
                                              (call [this control]
                                                (let [^Labeled control control]
                                                  (.textProperty control)))))


(defn validation-support
  "controlsfx validation registrator"
  ^ValidationSupport [^ValidationDecoration decoration]
  (doto (ValidationSupport.)
    (.setErrorDecorationEnabled true)
    (.setValidationDecorator decoration)))

(defn validate-control
  "register validation for a given control. when pred false control displays validation message.
  pred is a (fn [value] (Boolean.))"
  [^ValidationSupport validation ^Control control ^IFn pred ^String message]
  (.registerValidator validation control false (Validator/createPredicateValidator
                                                 (reify Predicate
                                                   (test [this val]
                                                     (boolean (pred val))))
                                                 message
                                                 Severity/ERROR)))

; property as clojure ref
(comment @(fx/property-ref node :text))
; lookup by id
(comment (fx/lookup node :#id))
(comment (load-fxml "main.fxml"))

(defn set-widget-children [node children]
  (fx/run!
    (.. node
        (getChildren)
        (setAll
          children))))

(defn resize-observable-list
  "resizes given list to new-size, if needed constructs new elements
  construct-fn is a (fn [index] (new-element-at-index))"
  [^ObservableList list new-size construct-fn]
  (let [old-size (count list)
        diff (- new-size old-size)]
    (if (<= 0 diff)
      (.addAll list ^Collection (for [i (range diff)]
                                  (construct-fn (+ old-size i))))
      (.remove list new-size old-size))))

(defn on-changed
  "registers a change callback on an observable value
  callback is a (fn [observable old-val new-val])"
  [^ObservableValue val callback]
  (let [listener (reify ChangeListener
                   (changed [this obs old new]
                     (callback obs old new)))]
    (.addListener val
                  listener)
    listener))

(defn observable-list
  "creates a new instance of observable array list from given collection"
  ([]
   (FXCollections/observableArrayList))
  ([^Collection col]
   (FXCollections/observableArrayList col)))

(defn ctrl-key-accelerator [^KeyCode code]
  (KeyCodeCombination. code (into-array (list KeyCodeCombination/SHORTCUT_DOWN))))

(defn on-text-field-confirmed
  "textfield-fn is called when focus lost, or field confirmed using [enter]
  textfield-fn is (fn [text-field])"
  [^TextField text-field textfield-fn]
  (on-changed (.focusedProperty text-field)
              (fn [observable old new]
                (when-not new
                  (textfield-fn text-field))))
  (.setOnAction text-field (reify EventHandler
                             (handle [this e]
                               (textfield-fn text-field)))))

(defn loose-bind [src-property ^WritableValue target-property]
  (on-changed src-property
              (fn [obs old new]
                (when (not= old new)
                  (.setValue target-property new)))))

(defn str-to-file [^File base-dir s]
  (let [^File file (io/file s)]
    (if (fs/absolute? file)
      file
      (fs/normalized (fs/file base-dir file)))))

(defn make-fs-select-widget
  "browsing-fn is (fn [e] show-dialog)"
  [^File base-dir file-str text-field browsing-fn]
  (let [relative-btn (ToggleButton. "Relative")
        absolute-btn (ToggleButton. "Absolute")
        absolute-relative-toggle (SegmentedButton.
                                   (observable-list (list relative-btn
                                                          absolute-btn)))
        ]
    (on-text-field-confirmed text-field (fn [text-field]
                                          (.setValue file-str (.getText text-field))))
    (loose-bind file-str (.textProperty text-field))
    (on-changed (.. absolute-relative-toggle
                    getToggleGroup
                    selectedToggleProperty)
                (fn [obs old type]
                  (let [file ^File (io/file (.getValue file-str))
                        new-file (if (= type relative-btn)
                                   (if (fs/absolute? file)
                                     (core/relativize-path base-dir file)
                                     file)
                                   (str-to-file base-dir file))]
                    (.setValue file-str (str new-file))
                    )))
    (on-changed file-str
                (fn [obs old new]
                  (when (not= old new)
                    (let [file (io/file new)]
                      (if (fs/absolute? file)
                        (.setSelected absolute-btn true)
                        (.setSelected relative-btn true))))))
    (doto (fx/h-box {:spacing 8}
                    (fx/button {:text "Browse" :on-action browsing-fn})
                    (doto text-field
                      (HBox/setHgrow Priority/ALWAYS))
                    absolute-relative-toggle))
    ))

(defn make-directory-select-widget [^File relative-to file-str-property description ^ValidationSupport validation]
  (let [text-field (fx/text-field {})]
    (validate-control validation text-field
                      (fn [value]
                        (let [file (str-to-file relative-to value)]
                          (and (.exists file) (.isDirectory file))
                          ))
                      "You must select an existing directory.")
    (make-fs-select-widget relative-to file-str-property text-field
                           (fn [e]
                             (let [file (.showDialog
                                          (doto (DirectoryChooser.)
                                            (.setInitialDirectory (let [dir (str-to-file relative-to (.getValue file-str-property))]
                                                                    (if (and (.exists dir) (.isDirectory dir))
                                                                      dir
                                                                      relative-to)))
                                            (.setTitle description)
                                            )
                                          nil)]
                               (when file
                                 (.setValue file-str-property (str file)))
                               )))))

(deftype DirPropertyItemEditor [widget ^String name obj-property]
  PropertyEditor
  (getEditor [this]
    widget)
  PropertySheet$Item
  (getType [this]
    String)
  (getCategory [this])
  (getName [this]
    name)
  (getDescription [this])
  (getValue [this]
    (.getValue obj-property))
  (setValue [this new-val]
    (.setValue obj-property new-val)))

(defn make-dir-property-item-editor [^String name ^File relative-to ^ValidationSupport validation on-update-fn]
  (let [property (SimpleStringProperty. "")
        widget (make-directory-select-widget relative-to property (str "Select " name " location") validation)]

    (on-changed property
                (fn [obs old new]
                  (when (not= old new)
                    (on-update-fn))))
    (->DirPropertyItemEditor widget name property)
    ))

(defn make-property-editor-factory []
  (let [default-factory (DefaultPropertyEditorFactory.)]
    (reify Callback
      (call [this item]
        (if (instance? PropertyEditor item)
          item
          (.call default-factory item))))))

(def property-editor-factory (make-property-editor-factory))

(deftype PropertyItem [^String name ^String description ^Class class obj-property on-update-fn editable]
  PropertySheet$Item
  (getType [this]
    class)
  (getCategory [this])
  (getName [this]
    name)
  (getDescription [this]
    description)
  (getValue [this]
    (.getValue obj-property))
  (setValue [this new-val]
    (when (not= (.getValue this) new-val)
      (.setValue obj-property new-val)
      (on-update-fn)))
  (isEditable [this]
    (boolean editable)))

;(fx/sandbox #(make-directory-select-widget (fs/file ".") (SimpleStringProperty. "") "Select working directory dir" (validation-support (StyleClassValidationDecoration.))))

