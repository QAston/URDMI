(ns urdmi.plugin.aleph.settings-page
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core])
  (:import (org.controlsfx.control PropertySheet PropertySheet$Mode)
           (javafx.beans.property SimpleObjectProperty)
           (org.controlsfx.validation.decoration StyleClassValidationDecoration)
           (java.io File)))


(defn make-widget [properties-list]
  (doto (PropertySheet. properties-list)
    (.setModeSwitcherVisible false)
    (.setSearchBoxVisible false)
    (.setMode PropertySheet$Mode/NAME)
    (.setPropertyEditorFactory gui/property-editor-factory)
    ))

(def fields [:aleph-loc :swi-prolog-loc])

(deftype AlephSettingsPage [widget properties-map current-page]
  gui/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key]
    (reset! current-page nil)
    (let [data (:data (get-in project (apply core/dir-keys key)))]
      (.setValue (:aleph-loc properties-map) (:aleph-loc data))
      (.setValue (:swi-prolog-loc properties-map) (:swi-prolog-loc data)))
    (reset! current-page key))
  (read-data [this]
    {:data {:aleph-loc      (.getValue (:aleph-loc properties-map))
            :swi-prolog-loc (.getValue (:swi-prolog-loc properties-map))
            }}))

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support (StyleClassValidationDecoration.))
        current-page (atom nil)
        on-update-fn (fn []
                       (when-let [key @current-page]
                         (async/put! >ui-requests {:type     :modified-page
                                                   :data-key key})))
        properties-map {:aleph-loc      (gui/make-file-property-item-editor "Aleph.pl"
                                                                            (:project-dir project)
                                                                            validation
                                                                            (fn [^File f]
                                                                              true)
                                                                            on-update-fn)
                        :swi-prolog-loc (gui/make-file-property-item-editor "Swiprolog plcon executable"
                                                                            (:project-dir project)
                                                                            validation
                                                                            (fn [^File f]
                                                                              true)
                                                                            on-update-fn)}
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget properties-list)]
    (->AlephSettingsPage widget properties-map current-page)))

#_{:target-rel       ["pracownik" 7]
   :target-rel-param 7
   :aleph-loc        "C:\\portable\\aleph.pl"
   :swi-prolog-loc   "C:\\Program Files\\pl\\bin\\plcon.exe"}