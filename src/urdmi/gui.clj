(ns urdmi.gui
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [fx-clj.core :as fx])
  (:import (javafx.collections ObservableList FXCollections)
           (java.util Collection)
           (javafx.beans.value ObservableValue ChangeListener)
           (org.controlsfx.validation ValidationSupport Validator Severity)
           (org.controlsfx.validation.decoration ValidationDecoration)
           (javafx.scene.control Control TableCell Labeled)
           (java.util.function Predicate)
           (clojure.lang IFn)
           (javafx.util Callback)
           (org.controlsfx.tools ValueExtractor)))

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



