(ns urdmi.gui-util
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [me.raynes.fs :as fs]
            [urdmi.util :as util]
            [clojure.set :as set])
  (:import (javafx.collections ObservableList FXCollections ListChangeListener ObservableMap MapChangeListener)
           (java.util Collection List Map)
           (javafx.beans.value ObservableValue ChangeListener WritableValue)
           (org.controlsfx.validation ValidationSupport Validator Severity)
           (org.controlsfx.validation.decoration ValidationDecoration)
           (javafx.scene.control Control TableCell Labeled TextField)
           (java.util.function Predicate)
           (clojure.lang IFn ISeq)
           (javafx.util Callback StringConverter)
           (org.controlsfx.tools ValueExtractor Borders)
           (javafx.scene.input KeyCodeCombination KeyCode KeyEvent)
           (javafx.event EventHandler ActionEvent)
           (javafx.scene.control ToggleButton ChoiceBox ComboBox Tooltip)
           (java.io File)
           (javafx.scene.layout HBox Priority)
           (javafx.stage DirectoryChooser FileChooser)
           (org.controlsfx.validation ValidationSupport)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty)
           (org.controlsfx.control SegmentedButton PropertySheet$Item)
           (org.controlsfx.property.editor PropertyEditor DefaultPropertyEditorFactory)
           (javafx.scene Node)))

;implementation level abstraction, gui taking viewmodel
(defprotocol DataWidget
  (get-node [this])
  (set-data! [this data data-key])
  (get-data [this]))

(defn init-toolkit []
  (javafx.embed.swing.JFXPanel.)
  (ValueExtractor/addObservableValueExtractor (reify Predicate
                                                (test [this control]
                                                  (instance? Labeled control)))
                                              (reify Callback
                                                (call [this control]
                                                  (let [^Labeled control control]
                                                    (.textProperty control))))))

(when (core/dev?)
  (defonce t (init-toolkit)))

(defn hover-decoration []
  (reify ValidationDecoration
    (removeDecorations [this control]
      (.setTooltip control nil)
      (.remove (.getStyleClass control) "error")
      )
    (applyValidationDecoration [this validation-message]
      (.setTooltip (.getTarget validation-message) (Tooltip. (.getText validation-message)))
      (.add (.getStyleClass (.getTarget validation-message)) "error"))
    (applyRequiredDecoration [this control])))

(defn default-stylesheet [^Node node]
  (.. node getStylesheets (add (.toExternalForm (io/resource "main.css")))))

(defn validation-support
  "controlsfx validation registrator"
  (^ValidationSupport [^ValidationDecoration decoration]
   (doto (ValidationSupport.)
     (.setErrorDecorationEnabled true)
     (.setValidationDecorator decoration)))
  (^ValidationSupport []
   (validation-support (hover-decoration))))

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
                     (when (not= old new)
                       (callback obs old new))))]
    (.addListener val
                  listener)
    listener))

(defn observable-list
  "creates a new instance of observable array list from given collection"
  ([]
   (FXCollections/observableArrayList))
  ([^Collection col]
   (FXCollections/observableArrayList col)))

(defn sync-list
  "updates list to have contents of sync-with, but without unnecesary changes to the list"
  [^List list-to-sync sync-with]
  (let [sync-with (set sync-with)
        list-set (set list-to-sync)
        to-add (set/difference sync-with list-set)
        to-remove (set/difference list-set sync-with)]
    (.addAll list-to-sync to-add)
    (.removeAll list-to-sync to-remove)))

(defn observable-map
  "creates a new instance of observable map from given map"
  ([]
   (FXCollections/observableHashMap))
  ([^Map col]
   (FXCollections/observableMap col)))

(defn ctrl-key-accelerator [^KeyCode code]
  (KeyCodeCombination. code (into-array (list KeyCodeCombination/SHORTCUT_DOWN))))

(defn trigger-action-on-app-shortcut [^Node node fn]
  (let [keys #{KeyCode/CONTROL KeyCode/COMMAND}]
    (.addEventFilter node KeyEvent/KEY_PRESSED (reify EventHandler
                                                 (handle [this e]
                                                   (when (keys (.getCode ^KeyEvent e))
                                                     (fn node))
                                                   )))))

(defn on-text-field-confirmed
  "textfield-fn is called when focus lost, or field confirmed using [enter]
  textfield-fn is (fn [text-field])"
  [^TextField text-field textfield-fn]

  (on-changed (.focusedProperty text-field)
              (fn [observable old new]
                (when-not new
                  (textfield-fn text-field))))
  (trigger-action-on-app-shortcut text-field textfield-fn)
  (.setOnAction text-field (reify EventHandler
                             (handle [this e]
                               (textfield-fn text-field)))))

(defn loose-bind [src-property ^WritableValue target-property]
  (on-changed src-property
              (fn [obs old new]
                (when (not= (.getValue target-property) new)
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

(defn make-executable-select-widget [^File base-dir file-str description ^ValidationSupport validation validate-fn invalid-msg]
  (let [text-field (fx/text-field {})
        relative-btn (ToggleButton. "Relative")
        absolute-btn (ToggleButton. "Absolute")
        path-btn (ToggleButton. "PATH")
        absolute-relative-toggle (SegmentedButton.
                                   ^ObservableList (observable-list (list path-btn
                                                                          relative-btn
                                                                          absolute-btn
                                                                          )))
        browsing-fn (fn [e]
                      (let [file (.showOpenDialog
                                   (doto (FileChooser.)
                                     (.setInitialDirectory (let [dir (str-to-file base-dir (.getValue file-str))]
                                                             (if (and (.exists dir) (.exists (fs/parent dir)))
                                                               (fs/parent dir)
                                                               base-dir)))
                                     (.setTitle description)
                                     )
                                   nil)]
                        (when file
                          (.setValue file-str (str file)))
                        ))]
    (on-text-field-confirmed text-field (fn [text-field]
                                          (.setValue file-str (.getText text-field))))
    (loose-bind file-str (.textProperty text-field))
    (on-changed (.. absolute-relative-toggle
                    getToggleGroup
                    selectedToggleProperty)
                (fn [obs old type]
                  (let [file ^File (io/file (.getValue file-str))
                        new-file (cond (= type path-btn) (fs/base-name file)

                                       (= type relative-btn) (let [relative (if (fs/absolute? file) (core/relativize-path base-dir file) file)]
                                                               (if (> (.getNameCount (.toPath relative)) 1)
                                                                 relative
                                                                 (io/file "." relative)))

                                       true (str-to-file base-dir file))]
                    (.setValue file-str (str new-file))
                    )))
    (on-changed file-str
                (fn [obs old new]
                  (when (not= old new)
                    (let [file (io/file new)]
                      (cond (fs/absolute? file) (.setSelected absolute-btn true)
                            (> (.getNameCount (.toPath file)) 1) (.setSelected relative-btn true)
                            true (.setSelected path-btn true)
                            )))))

    (validate-control validation text-field
                      (fn [value]
                        (validate-fn value)
                        )
                      invalid-msg)
    (doto (fx/h-box {:spacing 8}
                    (fx/button {:text "Browse" :on-action browsing-fn})
                    (doto text-field
                      (HBox/setHgrow Priority/ALWAYS))
                    absolute-relative-toggle))
    ))

(defn make-absolute-directory-select-widget [^File starting-dir file-str-property description ^ValidationSupport validation validation-fn invalid-msg]
  (let [text-field (fx/text-field {})]
    (on-text-field-confirmed text-field (fn [text-field]
                                          (.setValue file-str-property (.getText text-field))))
    (loose-bind file-str-property (.textProperty text-field))
    (validate-control validation text-field validation-fn invalid-msg)
    (doto (fx/h-box {:spacing 8}
                    (fx/button {:text "Browse" :on-action (fn [e]
                                                            (let [file (.showDialog
                                                                         (doto (DirectoryChooser.)
                                                                           (.setInitialDirectory starting-dir)
                                                                           (.setTitle description)
                                                                           )
                                                                         nil)]
                                                              (when file
                                                                (.setValue file-str-property (str file)))
                                                              ))})
                    (doto text-field
                      (HBox/setHgrow Priority/ALWAYS))))))

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
                  (on-update-fn)))
    (->PropertyItemEditor widget name property)))

(defn make-file-property-item-editor [^String name ^File relative-to ^ValidationSupport validation validate-fn on-update-fn]
  (let [property (SimpleStringProperty. "")
        widget (make-file-select-widget relative-to property (str "Select " name " location") validation validate-fn)]

    (on-changed property
                (fn [obs old new]
                  (on-update-fn)))
    (->PropertyItemEditor widget name property)))

(defn make-executable-item-editor [^String name ^File relative-to ^ValidationSupport validation validate-fn invalid-msg on-update-fn]
  (let [property (SimpleStringProperty. "")
        widget (make-executable-select-widget relative-to property (str "Select " name " location") validation validate-fn invalid-msg)]

    (on-changed property
                (fn [obs old new]
                  (on-update-fn)))
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

(defn choice-box [list selected]
  (let [widget (doto (ChoiceBox.)
                 (.setItems list))
        set-selection (fn [new]
                        (.setValue selected (first (filter #(= new %) list))))
        listener (reify ChangeListener
                   (changed [this obs old new]
                     (when-not (identical? old new)
                       (set-selection new))))]
    (.addListener list
                  (reify ListChangeListener
                    (onChanged [this change]
                      (set-selection (.getValue selected)))))
    (.addListener selected listener)
    (.bindBidirectional (.valueProperty widget) selected)
    widget
    ))

(defn text-combo-box [list selected]
  (let [widget (doto (ComboBox.)
                 (.setItems list)
                 (.setEditable true))]
    (on-text-field-confirmed (.getEditor widget)
                             (fn [text-field]
                               (.setValue selected (.getText text-field))))
    (.bindBidirectional (.valueProperty widget) selected)
    widget
    ))

(def relation-string-converter (proxy [StringConverter] []
                                 (fromString [s])
                                 (toString [v]
                                   (str (first v) "/" (second v)))))

(defn make-relation-select-widget [relations-list selected-relation validation]
  (let [widget (doto (choice-box relations-list selected-relation)
                 (.setConverter relation-string-converter))
        ]
    (when validation
      (validate-control validation
                        widget
                        (fn [val]
                          (boolean val)) "You must select a relation"))
    widget
    ))

(defn relation-term-string-converter [relations-colnames]
  (proxy [StringConverter] []
    (fromString [s])
    (toString [v]
        (str v ": " (get relations-colnames v)))))

(defn border-wrap [node name]
  (.. (Borders/wrap
        node)
      (lineBorder)
      (title name)
      (build)
      (build)))

(defprotocol PMutableToImut
  (to-imut [this]))

(defprotocol PMutableFromImut
  (from-imut [this imut]))

(extend-type ObservableList
  PMutableToImut
  (to-imut [this]
    (vec this))
  PMutableFromImut
  (from-imut [this imut]
    (.setAll this (vec imut))))

(extend-type Map
  PMutableToImut
  (to-imut [this]
    (into {} this))
  PMutableFromImut
  (from-imut [this imut]
    (if imut
      (do
        (.clear this)
        (.putAll this imut))
      (.clear this))
    ))

(extend-type WritableValue
  PMutableFromImut
  (from-imut [this imut]
    (.setValue this imut)))

(extend-type ObservableValue
  PMutableToImut
  (to-imut [this]
    (.getValue this)))

(defn map-of-mut-to-map-of-imut [map-of-mut]
  (into {}
        (for [[k v] map-of-mut]
          [k (to-imut v)])))

(defn map-of-mut-from-map-of-imut [map-of-mut map-of-imut]
  (doseq [[k v] map-of-mut]
    (from-imut v (k map-of-imut))))

(defprotocol PAnyObservable
  (on-any-change [this f]))

(extend-type ObservableValue
  PAnyObservable
  (on-any-change [this f]
    (on-changed this (fn [obs old new]
                       (f)))))

(extend-type ObservableList
  PAnyObservable
  (on-any-change [this f]
    (.addListener this (reify ListChangeListener
                         (onChanged [this c]
                           (while (.next c))
                           (f))))))

(extend-type ObservableMap
  PAnyObservable
  (on-any-change [this f]
    (.addListener this (reify MapChangeListener
                         (onChanged [this c]
                           (f))))))

(extend-type Object
  PAnyObservable
  (on-any-change [this f]))

(defn map-of-mut-on-any-change [map-of-mut f]
  (doseq [[k v] map-of-mut]
    (on-any-change v f)))

(defn make-relation-term-select-widget [selected-relation selected-relation-term relations-colnames validation]
  (let [arity (if-let [arity (second (.getValue selected-relation))]
                arity
                0)
        relation-term-list (observable-list (for [i (range arity)]
                                              i))
        term-widget (doto (choice-box relation-term-list selected-relation-term)
                      (.setMaxWidth 150.0)
                      (.setMinWidth 100.0)
                      )
        update-converter (fn [] (when (and (.getValue selected-relation) (get relations-colnames (.getValue selected-relation)))
                                  (.setConverter term-widget
                                                 (relation-term-string-converter (get relations-colnames (.getValue selected-relation))))))]


    (on-any-change relations-colnames update-converter)
    (when validation
      (validate-control validation
                        term-widget
                        (fn [val]
                          (boolean val)) "You must select a term"))

    (on-changed selected-relation
                (fn [obs old new]
                  (update-converter)
                  (when (not= old new)
                    (resize-observable-list relation-term-list (if new (second new) 0) identity)
                    (.setValue selected-relation-term (let [old-val (.getValue selected-relation-term)
                                                            new-val (second new)]
                                                        (when (and old-val new-val)
                                                          (min old-val (dec new-val)))))
                    )))
    term-widget))

(defn make-relation-and-term-select-widget
  [relation-list selected-relation selected-relation-term relations-colnames validation]
  (let [relation-widget (make-relation-select-widget relation-list selected-relation validation)
        relation-term-widget (make-relation-term-select-widget selected-relation selected-relation-term relations-colnames validation)
        widget (fx/h-box {}
                         (doto relation-widget
                           (HBox/setHgrow Priority/ALWAYS)
                           (.setMaxWidth Double/MAX_VALUE))
                         relation-term-widget)
        ]
    widget))