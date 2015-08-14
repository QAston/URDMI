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
                                                    (pred val)))
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
  (.addListener val
                (reify ChangeListener
                  (changed [this obs old new]
                    (callback obs old new)))))

(defn observable-list
  "creates a new instance of observable array list from given collection"
  ([]
   (FXCollections/observableArrayList))
  ([^Collection col]
  (FXCollections/observableArrayList col)))



