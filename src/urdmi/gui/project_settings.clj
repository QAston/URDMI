(ns urdmi.gui.project-settings
  (:require [fx-clj.core :as fx]
            [urdmi.gui-util :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io])
  (:import (org.controlsfx.control PropertySheet PropertySheet$Mode PropertySheet$Item SegmentedButton)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty SimpleBooleanProperty)
           (org.controlsfx.validation.decoration StyleClassValidationDecoration)))


(defn make-widget [properties-list]
  (doto (PropertySheet. properties-list)
    (.setModeSwitcherVisible false)
    (.setSearchBoxVisible false)
    (.setMode PropertySheet$Mode/NAME)
    (.setPropertyEditorFactory gui/property-editor-factory)
    ))

(def fields [:active-plugin :working-dir])

(deftype ProjectSettingsPage [widget properties-map user-input]
  core/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (reset! user-input false)
    (when modified
      (let [data @(:data (get-in project (apply core/model-map-keys key)))]
        (.setValue (:active-plugin properties-map) (name (:active-plugin data)))
        (.setValue (:working-dir properties-map) (str (:working-dir data)))))
    (reset! user-input true))
  (read-data [this]
    (core/file-item {:active-plugin (keyword (.getValue (:active-plugin properties-map)))
             :working-dir   (io/file (.getValue (:working-dir properties-map)))})))

(defn make-page [>ui-requests project]
  (let [user-input (atom false)
        on-update-fn (fn []
                       (when @user-input
                         (async/put! >ui-requests {:type     :modified-page})))
        properties-map {:active-plugin (gui/->PropertyItem "Active Plugin"
                                                           "Curently active dataminging plugin"
                                                           String
                                                           (SimpleObjectProperty. "")
                                                           on-update-fn
                                                           false)
                        :working-dir (gui/make-dir-property-item-editor  "Build directory"
                                                                         (:project-dir project)
                                                                         (gui/validation-support)
                                                                         on-update-fn)}
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget properties-list)]
    (->ProjectSettingsPage widget properties-map user-input)))