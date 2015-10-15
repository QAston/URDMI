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
           (javafx.collections ListChangeListener ObservableList)
           (javafx.scene.layout GridPane ColumnConstraints HBox Priority RowConstraints)
           (javafx.geometry Pos HPos VPos)
           (java.util List)
           (javafx.beans.binding BooleanExpression ObjectExpression ListExpression)
           (javafx.util Callback StringConverter)
           (javafx.scene.control.cell TextFieldListCell)))


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
              target-rel (:target-rel data)
              joinable-rels (remove #{target-rel} relations)
              {:keys [relation relation-list]} (.getValue (:target-term properties-map))
              {:keys [joinable-relations target-relation-index joined-relations]} (.getValue (:models-format properties-map))]
          (.setAll relation-list relations)
          (.setValue (:ace-loc properties-map) (:ace-loc data))
          (.setValue relation target-rel)
          (.setValue (:command properties-map) (:command data))
          (.setValue (:kb-format properties-map) (:kb-format data))
          (.setValue target-relation-index (:target-relation-index (:models-format data)))
          (.setAll joined-relations (:joined-relations (:models-format data)))
          (.setAll joinable-relations joinable-rels)
          )
        ; just modify relation list
        (let [{:keys [relation relation-list]} (.getValue (:target-term properties-map))
              {:keys [joinable-relations]} (.getValue (:models-format properties-map))
              model-relations (set (map :rel (core/get-relations project)))
              edited-relations (set relation-list)
              to-add (set/difference model-relations edited-relations)
              to-remove (set/difference edited-relations model-relations)]
          (.removeAll relation-list to-remove)
          (.addAll relation-list to-add)
          (.removeAll joinable-relations to-remove)
          (.addAll joinable-relations to-add))
        )
      (reset! current-page key)))
  (read-data [this]
    (let [{:keys [relation]} (.getValue (:target-term properties-map))]
      (core/file-item {:ace-loc    (.getValue (:ace-loc properties-map))
                       :target-rel (.getValue relation)
                       :command    (.getValue (:command properties-map))
                       :kb-format  (.getValue (:kb-format properties-map))
                       :models-format (let [data (.getValue (:models-format properties-map))]
                                        (-> data
                                          (dissoc  :joinable-relations)
                                            (update :target-relation-index (memfn getValue))))
                       }))))

(defn combo-box [list selected]
  (let [widget (doto (ComboBox.)
                 (.setItems list)
                 (.setEditable true))]
    (.bindBidirectional (.valueProperty widget) selected)
    widget
    ))

(defn make-rel-term-list [^ObservableList relations-list ^ObservableList selected-relations-list validation]
  (let [
        selected-rels-widget (doto (fx/list-view {:max-height 200.0
                                                  :min-width 300.0})
                               (.setItems selected-relations-list)
                               (.setCellFactory (reify Callback
                                                    (call [this list-view]
                                                      (TextFieldListCell. (proxy [StringConverter] []
                                                                            (fromString [s])
                                                                            (toString [[v t]]
                                                                              (str (first v) "_" (second v) ".pl" " " t)))))))
                               )
        unselected-relations (doto (gui/observable-list)
                               (.addAll relations-list)
                               (.removeAll (map first selected-relations-list)))
        added-fn (fn [^long idx]
                   (let [[selected-relation selected-relation-term :as rel-item] (.get selected-relations-list idx)]
                     (.remove unselected-relations selected-relation)
                     ))
        removed-fn (fn [idx [selected-relation selected-relation-term]]
                     (.add unselected-relations selected-relation)
                     )
        rel-to-add (SimpleObjectProperty.)
        term-to-add (SimpleObjectProperty.)
        select-rel-widget (gui/make-relation-and-term-select-widget unselected-relations rel-to-add term-to-add validation)
        add-widget (fx/h-box {}
                             (doto select-rel-widget
                               (HBox/setHgrow Priority/ALWAYS))
                             (doto
                               (fx/button {:text      "Add"
                                           :disable   (.or (.isNull (ObjectExpression/objectExpression rel-to-add))
                                                            (.isNull (ObjectExpression/objectExpression term-to-add)))
                                           :on-action (fn [e]
                                                        (.add selected-relations-list
                                                              [(.getValue rel-to-add) (.getValue term-to-add)]))})
                               (HBox/setHgrow Priority/NEVER))
                             (doto
                               (fx/button {:text "Del"
                                           :disable   (.isNull (ObjectExpression/objectExpression (.. selected-rels-widget getSelectionModel selectedItemProperty)))
                                           :on-action
                                                 (fn [e] (.removeAll selected-relations-list (.. selected-rels-widget getSelectionModel getSelectedItems)))})))
        widget (fx/v-box {}
                         add-widget
                         selected-rels-widget
                         )
        ]
    (doseq [idx (range 0 (count selected-relations-list))]
      (added-fn idx))
    (.addListener selected-relations-list
                  (reify ListChangeListener
                    (onChanged [this change]
                      (while (.next change)
                        (when (.wasRemoved change)
                          (let [removed (.getRemoved change)]
                            (doseq [^long idx (range (.getFrom change) (+ (.getFrom change) (.getRemovedSize change)))]
                              (removed-fn idx (.get removed (- idx (.getFrom change))))
                              )))
                        (when (.wasAdded change)
                          (doseq [^long idx (range (.getFrom change) (.getTo change))]
                            (added-fn idx)
                            ))))))
    (.addListener relations-list
                  (reify ListChangeListener
                    (onChanged [this change]
                      (while (.next change)
                        (when (.wasAdded change)
                          (let [added (.getAddedSubList change)]
                            (.addAll unselected-relations added)))
                        (when (.wasRemoved change)
                          (let [removed (.getRemoved change)
                                sel (vec selected-relations-list)
                                rem-set (set removed)]
                            (.setAll selected-relations-list (remove (fn [[rel term]]
                                                                       (rem-set rel)) sel))
                            (.removeAll unselected-relations removed)))))))
    widget))

(defn models-format-settings-widget [target-relation target-relation-index relations-list joined-relations validation]
  (let [term-widget (gui/make-relation-term-select-widget target-relation target-relation-index validation)
        grid (doto (GridPane.)
               (.setAlignment Pos/CENTER)
               (.setPrefWidth 500.0)
               (.setHgap 10.0)
               (.setVgap 12.0)
               (.. getColumnConstraints
                   (setAll [(doto (ColumnConstraints.)
                              (.setHalignment HPos/LEFT))
                            (doto (ColumnConstraints.)
                              (.setHalignment HPos/RIGHT))
                            ]))
               (.. getRowConstraints
                   (setAll [(doto (RowConstraints.)
                              (.setValignment VPos/TOP))
                            (doto (RowConstraints.)
                              (.setValignment VPos/TOP))]))
               (.add (fx/label {:text "Target rel index term"}) 0 0)
               (.add term-widget 1 0)
               (.add (fx/label {:text "Join rels by index"}) 0 1)
               (.add (make-rel-term-list relations-list joined-relations validation) 1 1)
               )]
    grid))


(def fields [:ace-loc :target-term :command :kb-format :models-format])

;knowledgebase format: model, key

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support (StyleClassValidationDecoration.))
        current-page (atom nil)
        on-update-fn (fn []
                       (when-let [key @current-page]
                         (async/put! >ui-requests {:type     :modified-page
                                                   :data-key key})))

        kb-formats-list (gui/observable-list (list ace/knowledgebase-key ace/knowledgebase-models))
        kb-selected-format (SimpleStringProperty. ace/knowledgebase-key)
        kb-format-widget (gui/choice-box kb-formats-list kb-selected-format)
        joined-relations (gui/observable-list)
        relation-list (gui/observable-list)
        joinable-relations (gui/observable-list)
        target-relation (SimpleObjectProperty.)
        command-property (SimpleStringProperty.)
        command-widget (combo-box (gui/observable-list (sort ace/example-commands)) command-property)

        target-relation-index (SimpleObjectProperty. 0)
        models-format-widget (models-format-settings-widget target-relation target-relation-index joinable-relations joined-relations validation)


        properties-map {:ace-loc       (gui/make-executable-item-editor "Ace executable"
                                                                           (:project-dir project)
                                                                           validation
                                                                           (fn [s]
                                                                             (ace/check-ace-path (core/resolve-executable-loc (:project-dir project) s)))
                                                                           on-update-fn)
                        :target-term   (gui/->PropertyItemEditor
                                         (gui/make-relation-select-widget relation-list target-relation validation)
                                         "Target relation"
                                         (SimpleObjectProperty. {:relation target-relation :relation-list relation-list}))

                        :command       (gui/->PropertyItemEditor command-widget "Mining command" command-property)
                        :kb-format     (gui/->PropertyItemEditor kb-format-widget "Knowledgebase format" kb-selected-format)
                        :models-format (gui/->PropertyItemEditor models-format-widget "Models format settings" (SimpleObjectProperty.
                                                                                                                 {:joinable-relations    joinable-relations
                                                                                                                  :joined-relations      joined-relations
                                                                                                                  :target-relation-index target-relation-index}))}
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget properties-list)]
    (.bind (.disableProperty models-format-widget) (.isNotEqualTo (ObjectExpression/objectExpression kb-selected-format) ace/knowledgebase-models))
    (gui/validate-control validation command-widget (fn [cmd]
                                                      (and cmd (not (.isEmpty cmd)))) "You must enter a command")
    (doseq [prop [target-relation command-property kb-selected-format]]
      (gui/on-changed prop
                      (fn [obs old new]
                        (on-update-fn))))
    (gui/on-changed target-relation
                    (fn [obs old new]
                      (when @current-page
                        (when old
                          (.add joinable-relations old))
                        (when new
                          (.remove joinable-relations new)))))
    (.addListener joined-relations
                  (reify ListChangeListener
                    (onChanged [this change]
                      (while (.next change)
                      (when (or (.wasRemoved change) (.wasAdded change))
                        (on-update-fn))
                      ))))
    (->AceSettingsPage widget properties-map current-page)))