(ns urdmi.gui.project-settings
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io])
  (:import (org.controlsfx.control PropertySheet PropertySheet$Mode PropertySheet$Item SegmentedButton)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty SimpleBooleanProperty)
           (java.io File)))


(defn make-widget [ui-requests properties-list]
  (doto (PropertySheet. properties-list)
    (.setModeSwitcherVisible false)
    (.setSearchBoxVisible false)
    (.setMode PropertySheet$Mode/NAME)
    ))

(defn make-item [[key value]]
  (reify PropertySheet$Item
    (getType [this])
    (getCategory [this])
    (getName [this])
    (getDescription [this])
    (getValue [this])
    (setValue [this new-val])
    (isEditable [this])
    (getPropertyEditorClass [this])))

(deftype StringItem [^String name ^String description ^Class class obj-property on-update-fn]
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
    true))

(def fields [:active-plugin :working-dir])

(deftype ProjectSettingsPage [widget properties-map current-page]
  gui/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key]
    (reset! current-page nil)
    (let [data (:data (get-in project (apply core/dir-keys key)))]
      (.setValue (:active-plugin properties-map) (name (:active-plugin data)))
      (.setValue (:working-dir properties-map) (str (:working-dir data))))
    (reset! current-page key))
  (read-data [this]
    {:data {:active-plugin (keyword (.getValue (:active-plugin properties-map)))
            :working-dir   (io/file (.getValue (:working-dir properties-map)))
            }}))

(defn make-page [>ui-requests]
  (let [current-page (atom nil)
        on-update-fn (fn []
                       (when-let [key @current-page]
                         (async/put! >ui-requests {:type     :modified-page
                                                   :data-key key})))
        properties-map {:active-plugin (->StringItem "Active Plugin"
                                                     "Curently active dataminging plugin"
                                                     String
                                                     (SimpleObjectProperty. "")
                                                     on-update-fn)
                        :working-dir   (->StringItem "Working directory location"
                                                     "Directory where giles for the datamining engine will be stored"
                                                     String
                                                     (SimpleObjectProperty. (io/file ""))
                                                     on-update-fn)}
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget >ui-requests properties-list)]
    (->ProjectSettingsPage widget properties-map current-page)))