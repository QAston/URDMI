(ns urdmi.gui
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [me.raynes.fs :as fs])
  (:import (javafx.collections ObservableList FXCollections ListChangeListener)
           (java.util Collection)
           (javafx.beans.value ObservableValue ChangeListener WritableValue)
           (org.controlsfx.validation ValidationSupport Validator Severity)
           (org.controlsfx.validation.decoration ValidationDecoration)
           (javafx.scene.control Control TableCell Labeled TextField)
           (java.util.function Predicate)
           (clojure.lang IFn)
           (javafx.util Callback StringConverter)
           (org.controlsfx.tools ValueExtractor)
           (javafx.scene.input KeyCodeCombination KeyCode)
           (javafx.event EventHandler)
           (javafx.scene.control ToggleButton ChoiceBox)
           (java.io File)
           (javafx.scene.layout HBox Priority)
           (javafx.stage DirectoryChooser FileChooser)
           (org.controlsfx.validation ValidationSupport)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty)
           (org.controlsfx.control SegmentedButton PropertySheet$Item)
           (org.controlsfx.property.editor PropertyEditor DefaultPropertyEditorFactory)))

(defn load-fxml [filename]
  (let [loader (new javafx.fxml.FXMLLoader (io/resource filename))]
    (.load loader)))

;(defonce _ (do
;application level abstraction, implements presenting data from app model
(defprotocol ContentPage
  (container-node [this])
  (show-data [this data data-key modified])
  (read-data [this]))

;implementation level abstraction, gui taking viewmodel
(defprotocol DataWidget
  (get-node [this])
  (set-data! [this data data-key])
  (get-data [this]))

(defprotocol PluginGui
  (new-project-creation-page [this >ui-requests] "Returns a view for creating a project using this plugin")
  (new-page [this project key >ui-requests] "Returns a view for editing/display of a menu entry")
  (model-modified [this project key] "A hook called after project model was modified."))

(ValueExtractor/addObservableValueExtractor (reify Predicate
                                              (test [this control]
                                                (instance? Labeled control)))
                                            (reify Callback
                                              (call [this control]
                                                (let [^Labeled control control]
                                                  (.textProperty control)))))

;))


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
                    absolute-relative-toggle))))

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

(defn make-file-select-widget [^File relative-to file-str-property description ^ValidationSupport validation validate-fn]
  (let [text-field (fx/text-field {})]
    (validate-control validation text-field
                      (fn [value]
                        (let [file (str-to-file relative-to value)]
                          (and (.exists file) (not (.isDirectory file)) (validate-fn file))
                          ))
                      "You must select an existing file.")
    (make-fs-select-widget relative-to file-str-property text-field
                           (fn [e]
                             (let [file (.showOpenDialog
                                          (doto (FileChooser.)
                                            (.setInitialDirectory (let [dir (str-to-file relative-to (.getValue file-str-property))]
                                                                    (if (and (.exists dir) (.exists (fs/parent dir)))
                                                                      (fs/parent dir)
                                                                      relative-to)))
                                            (.setTitle description)
                                            )
                                          nil)]
                               (when file
                                 (.setValue file-str-property (str file)))
                               )))))

(deftype PropertyItemEditor [widget ^String name obj-property]
  PropertyEditor
  (getEditor [this]
    widget)
  PropertySheet$Item
  (getType [this]
    Object)
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
    (->PropertyItemEditor widget name property)))

(defn make-file-property-item-editor [^String name ^File relative-to ^ValidationSupport validation validate-fn on-update-fn]
  (let [property (SimpleStringProperty. "")
        widget (make-file-select-widget relative-to property (str "Select " name " location") validation validate-fn)]

    (on-changed property
                (fn [obs old new]
                  (when (not= old new)
                    (on-update-fn))))
    (->PropertyItemEditor widget name property)))

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

(defn make-relation-select-widget [relations-list selected-relation validation]
  (let [widget (doto (ChoiceBox.)
                 (.setConverter (proxy [StringConverter] []
                                  (fromString [s])
                                  (toString [v]
                                    (str (first v) "_" (second v) ".pl"))))
                 (.setItems relations-list))
        ]
    (validate-control validation
                      widget
                      (fn [val]
                        (boolean val)) "Must select a valid binary term with 2 values")
    (.addListener relations-list
                  (reify ListChangeListener
                    (onChanged [this change]
                      (when-not (.contains relations-list (.getValue selected-relation))
                        (.setValue selected-relation nil))
                      )))
    (.bindBidirectional (.valueProperty widget) selected-relation)
    widget
    ))

(defn make-relation-term-select-widget [relation-list selected-relation selected-relation-term validation]
  (let [arity (if-let [arity (second (.getValue selected-relation))]
                arity
                0)
        relation-term-list (observable-list (for [i (range arity)]
                                              i))
        term-widget (doto (ChoiceBox.)
                      (.setItems relation-term-list)
                      )
        widget (fx/h-box {}
                         (make-relation-select-widget relation-list selected-relation validation)
                         term-widget)]
    (validate-control validation
                      term-widget
                      (fn [val]
                        (boolean val)) "Must select a valid binary term with 2 values")
    (on-changed selected-relation
                (fn [obs old new]
                  (when (not= old new)
                    (resize-observable-list relation-term-list (if new (second new) 0) identity)
                    (.setValue selected-relation-term (let [old-val (.getValue selected-relation-term)
                                                            new-val (second new)]
                                                        (when (and old-val new-val)
                                                          (min old-val (dec new-val)))))
                    )))
    (.bindBidirectional (.valueProperty term-widget) selected-relation-term)
    widget))

(defn make-target-term-item-editor [name validation on-update-fn]
  (let [selected-relation (SimpleObjectProperty. nil)
        relation-list (observable-list [])
        selected-relation-term (SimpleObjectProperty. nil)
        widget (make-relation-term-select-widget relation-list selected-relation selected-relation-term validation)]
    (on-changed selected-relation
                (fn [obs old new]
                  (when (not= old new)
                    (on-update-fn))))
    (on-changed selected-relation-term
                (fn [obs old new]
                  (when (not= old new)
                    (on-update-fn))))
    (->PropertyItemEditor widget name (SimpleObjectProperty. {:relation selected-relation :relation-list relation-list :relation-term selected-relation-term}))
    ))

