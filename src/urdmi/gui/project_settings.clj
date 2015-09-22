(ns urdmi.gui.project-settings
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core])
  (:import (org.controlsfx.control PropertySheet PropertySheet$Mode PropertySheet$Item)
           (java.util Optional)))


(defn make-widget[ui-requests]
  (doto (PropertySheet.)
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

(defn read-item [^PropertySheet$Item item]
  [(keyword (.getName item)) (.getValue item)])

(defn update-items! [observable-items-list data]
  ())

(deftype ProjectSettingsPage [widget]
  gui/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key]
    (let [data (get-in project (apply core/dir-keys key))]
      ()))
  (read-data [this]
    nil))

(defn make-page [ui-requests]
  (->ProjectSettingsPage (make-widget ui-requests)))

(fx/sandbox #(make-widget (async/chan)))
