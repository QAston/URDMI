(ns urdmi.plugin.ace.settings-page
   (:require [fx-clj.core :as fx]
             [urdmi.gui :as gui]
             [clojure.core.async :as async]
             [urdmi.core :as core])
   (:import (org.controlsfx.control PropertySheet PropertySheet$Mode)
            (org.controlsfx.validation.decoration StyleClassValidationDecoration)
            (java.io File)))


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
      (reset! current-page nil)
      (let [data (:data (get-in project (apply core/dir-keys key)))]
         (.setValue (:ace-loc properties-map) (:ace-loc data)))
      (reset! current-page key))
   (read-data [this]
      {:data {:ace-loc      (.getValue (:ace-loc properties-map))
              }}))

(def fields [:ace-loc])

(defn make-page [>ui-requests project]
   (let [validation (gui/validation-support (StyleClassValidationDecoration.))
         current-page (atom nil)
         on-update-fn (fn []
                         (when-let [key @current-page]
                            (async/put! >ui-requests {:type     :modified-page
                                                      :data-key key})))
         properties-map {:ace-loc      (gui/make-file-property-item-editor "Ace executable"
                                                                             (:project-dir project)
                                                                             validation
                                                                             (fn [^File f]
                                                                                true)
                                                                             on-update-fn)}
         properties-list (gui/observable-list (map properties-map fields))
         widget (make-widget properties-list)]
      (->AceSettingsPage widget properties-map current-page)))

#_{:target-rel ["pracownik" 7],
   :target-rel-param 7,
   :ace-loc "C:\\ML-Tools\\ACE-1.2.15\\windows\\bin\\ACE.exe"}