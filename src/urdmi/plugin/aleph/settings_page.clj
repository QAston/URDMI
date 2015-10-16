(ns urdmi.plugin.aleph.settings-page
  (:require [fx-clj.core :as fx]
            [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core]
            [clojure.set :as set]
            [urdmi.plugin.aleph.core :as aleph])
  (:import (org.controlsfx.control PropertySheet PropertySheet$Mode)
           (org.controlsfx.validation.decoration StyleClassValidationDecoration)
           (java.io File)
           (javafx.scene.control ChoiceBox)
           (javafx.collections ListChangeListener)
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
      (let [data (core/get-settings-data project (last key))
            relations (doall (map :rel (core/get-relations project)))
            target-rel (:target-rel data)
            {:keys [relation relation-term relation-list]} (.getValue (:target-term properties-map))]

        (.setAll relation-list relations)
        (.setValue (:aleph-loc properties-map) (:aleph-loc data))
        (.setValue (:swi-prolog-loc properties-map) (:swi-prolog-loc data))
        (.setValue relation target-rel)
        (.setValue relation-term (:target-rel-param data))
        (.setValue (:program properties-map) (get data :program "induce")))
      (let [{:keys [relation-list]} (.getValue (:target-term properties-map))
            model-relations (set (map :rel (core/get-relations project)))
            edited-relations (set relation-list)
            to-add (set/difference model-relations edited-relations)
            to-remove (set/difference edited-relations model-relations)]
        (.removeAll relation-list to-remove)
        (.addAll relation-list to-add)))
    (reset! user-input true)
    )
  (read-data [this]
    (let [{:keys [relation relation-term]} (.getValue (:target-term properties-map))]
      (core/file-item {:aleph-loc        (.getValue (:aleph-loc properties-map))
                       :swi-prolog-loc   (.getValue (:swi-prolog-loc properties-map))
                       :target-rel       (.getValue relation)
                       :target-rel-param (.getValue relation-term)
                       :program          (.getValue (:program properties-map))
                       }))))

(def fields [:aleph-loc :swi-prolog-loc :target-term :program])

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support)
        user-input (atom nil)
        on-update-fn (fn []
                       (when @user-input
                         (async/put! >ui-requests {:type     :modified-page})))
        program-property (SimpleStringProperty. "induce")
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
                        :target-term    (gui/make-target-term-item-editor "Target rel. term (values 0/1)"
                                                                          validation
                                                                          on-update-fn)
                        :program        (gui/->PropertyItemEditor (gui/choice-box (gui/observable-list aleph/programs) program-property) "Mining program" program-property)
                        }
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget properties-list)]
    (gui/on-changed program-property
                    (fn [obs old new]
                        (on-update-fn)))
    (->AlephSettingsPage widget properties-map user-input)))