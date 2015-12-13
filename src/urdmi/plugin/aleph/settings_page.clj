(ns urdmi.plugin.aleph.settings-page
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core]
            [clojure.set :as set]
            [urdmi.plugin.aleph.core :as aleph])
  (:import (org.controlsfx.control PropertySheet PropertySheet$Mode)
           (javafx.beans.property SimpleStringProperty)))


(defn make-widget [properties-list]
  (doto (PropertySheet. properties-list)
    (.setModeSwitcherVisible false)
    (.setSearchBoxVisible false)
    (.setMode PropertySheet$Mode/NAME)
    (.setPropertyEditorFactory gui/property-editor-factory)
    ))

(deftype AlephSettingsPage [widget properties-map user-input]
  gui/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (reset! user-input false)
    (if modified
      (let [data (core/get-settings-data project (last key))]
        (.setValue (:aleph-loc properties-map) (:aleph-loc data))
        (.setValue (:swi-prolog-loc properties-map) (:swi-prolog-loc data)))
      )
    (reset! user-input true)
    )
  (read-data [this]
      (core/file-item {:aleph-loc        (.getValue (:aleph-loc properties-map))
                       :swi-prolog-loc   (.getValue (:swi-prolog-loc properties-map))
                       })))

(def fields [:aleph-loc :swi-prolog-loc])

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support)
        user-input (atom nil)
        on-update-fn (fn []
                       (when @user-input
                         (async/put! >ui-requests {:type     :modified-page})))
        properties-map {:aleph-loc      (gui/make-file-property-item-editor "Aleph.pl"
                                                                            (:project-dir project)
                                                                            validation
                                                                            (fn [s]
                                                                              true)
                                                                            on-update-fn)
                        :swi-prolog-loc (gui/make-executable-item-editor "Swiprolog plcon executable"
                                                                         (:project-dir project)
                                                                         validation
                                                                         (fn [s]
                                                                           (aleph/check-plcon-path (core/resolve-executable-loc (:project-dir project) s)))
                                                                         "Could not find plcon executable in specified path"
                                                                         on-update-fn
                                                                         )
                        }
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget properties-list)]
    (->AlephSettingsPage widget properties-map user-input)))