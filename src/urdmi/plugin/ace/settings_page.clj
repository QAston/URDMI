(ns urdmi.plugin.ace.settings-page
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core]
            [clojure.set :as set]
            [urdmi.plugin.ace.core :as ace])
  (:import (org.controlsfx.control PropertySheet PropertySheet$Mode)
           (org.controlsfx.validation.decoration StyleClassValidationDecoration)
           (java.io File)
           (javafx.beans.property SimpleObjectProperty SimpleStringProperty)
           (javafx.scene.control ComboBox)
           (javafx.collections ListChangeListener)))


(defn make-widget [properties-list]
  (doto (PropertySheet. properties-list)
    (.setModeSwitcherVisible false)
    (.setSearchBoxVisible false)
    (.setMode PropertySheet$Mode/NAME)
    (.setPropertyEditorFactory gui/property-editor-factory)
    ))

(deftype AceSettingsPage [widget properties-map current-page]
  gui/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (fx/run!
      (reset! current-page nil)
      (if modified
        ; reload whole page
        (let [data (core/get-settings-data project (last key))
              relations (doall (map :rel (core/get-relations project)))
              target-rel (let [index (.indexOf relations (:target-rel data))]
                           (when-not (= index -1)
                             (.get relations index)))
              {:keys [relation relation-list]} (.getValue (:target-term properties-map))]
          (.setAll relation-list relations)
          (.setValue (:ace-loc properties-map) (:ace-loc data))
          (.setValue relation target-rel)
          (.setValue (:command properties-map) (:command data)))
        ; just modify relation list
        (let [{:keys [relation relation-list]} (.getValue (:target-term properties-map))
              model-relations (set (map :rel (core/get-relations project)))
              edited-relations (set relation-list)
              to-add (set/difference model-relations edited-relations)
              to-remove (set/difference edited-relations model-relations)]
          (.removeAll relation-list to-remove)
          (.addAll relation-list to-add))
        )
      (reset! current-page key)))
  (read-data [this]
    (let [{:keys [relation]} (.getValue (:target-term properties-map))]
      (core/file-item {:ace-loc          (.getValue (:ace-loc properties-map))
                       :target-rel       (.getValue relation)
                       :command          (.getValue (:command properties-map))
                       }))))

(defn combo-box [list selected]
  (let [widget (doto (ComboBox.)
                 (.setItems list))]
    (.addListener list
                  (reify ListChangeListener
                    (onChanged [this change]
                      (when-not (.contains list (.getValue selected))
                        (.setValue selected nil))
                      )))
    (.bindBidirectional (.valueProperty widget) selected)
    widget
    ))

(def fields [:ace-loc :target-term :command])

;knowledgebase format: model, key

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support (StyleClassValidationDecoration.))
        current-page (atom nil)
        on-update-fn (fn []
                       (when-let [key @current-page]
                         (async/put! >ui-requests {:type     :modified-page
                                                   :data-key key})))

        relation-list (gui/observable-list)
        selected-relation (SimpleObjectProperty.)
        command-property (SimpleStringProperty.)
        command-widget (combo-box (gui/observable-list (sort ace/example-commands)) command-property)

        properties-map {:ace-loc     (gui/make-file-property-item-editor "Ace executable"
                                                                         (:project-dir project)
                                                                         validation
                                                                         (fn [^File f]
                                                                           true)
                                                                         on-update-fn)
                        :target-term (gui/->PropertyItemEditor
                                       (gui/make-relation-select-widget relation-list selected-relation validation)
                                       "Target relation"
                                       (SimpleObjectProperty. {:relation selected-relation :relation-list relation-list}))

                        :command     (gui/->PropertyItemEditor command-widget "Mining command" command-property)}
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget properties-list)]
    (gui/validate-control validation command-widget (fn [cmd]
                                                      (and cmd (not (.isEmpty cmd)))) "You must enter a command")
    (gui/on-changed selected-relation
                    (fn [obs old new]
                      (on-update-fn)))
    (gui/on-changed command-property
                    (fn [obs old new]
                      (on-update-fn)))
    (->AceSettingsPage widget properties-map current-page)))