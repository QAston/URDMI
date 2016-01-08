(ns urdmi.plugin.ace.settings-page
  (:require [fx-clj.core :as fx]
            [urdmi.gui-util :as gui]
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
           (java.util List Collection)
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

(deftype AceSettingsPage [widget properties-map user-input]
  core/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (fx/run<!!
      (reset! user-input false)
      (let [{:keys [relation relation-list relations-colnames]} (.getValue (:target-term properties-map))]
        (gui/from-imut relations-colnames (core/generate-relation-columnname-map project)))
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
          (.setAll joinable-relations joinable-rels)
          (.setAll joined-relations (:joined-relations (:models-format data)))

          ))
        ; just modify relation list
        (let [{:keys [relation relation-list relations-colnames]} (.getValue (:target-term properties-map))
              {:keys [joinable-relations]} (.getValue (:models-format properties-map))
              model-relations (map :rel (core/get-relations project))]
          (gui/sync-list relation-list model-relations)
          (gui/sync-list joinable-relations model-relations)
          (gui/from-imut relations-colnames (core/generate-relation-columnname-map project))
        )
      (reset! user-input true)))
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

(defn make-rel-term-list [^ObservableList joinable-relations ^ObservableList joined-relations relations-colnames validation]
  (let [
        selected-rels-widget (doto (fx/list-view {:max-height 200.0
                                                  :min-width 300.0})
                               (.setItems joined-relations)

                               )
        unselected-relations (doto (gui/observable-list)
                               (.addAll joinable-relations)
                               (.removeAll (map first joined-relations)))
        added-fn (fn [^long idx]
                   (let [[selected-relation selected-relation-term :as rel-item] (.get joined-relations idx)]
                     (.remove unselected-relations selected-relation)
                     ))
        removed-fn (fn [idx [selected-relation selected-relation-term]]
                     (.add unselected-relations selected-relation)
                     )
        rel-to-add (SimpleObjectProperty.)
        term-to-add (SimpleObjectProperty.)
        select-rel-widget (gui/make-relation-and-term-select-widget unselected-relations rel-to-add term-to-add relations-colnames validation)
        add-widget (fx/h-box {}
                             (doto select-rel-widget
                               (HBox/setHgrow Priority/ALWAYS))
                             (doto
                               (fx/button {:text      "Add"
                                           :disable   (.or (.isNull (ObjectExpression/objectExpression rel-to-add))
                                                            (.isNull (ObjectExpression/objectExpression term-to-add)))
                                           :on-action (fn [e]
                                                        (.add joined-relations
                                                              [(.getValue rel-to-add) (.getValue term-to-add)]))})
                               (HBox/setHgrow Priority/NEVER))
                             (doto
                               (fx/button {:text "Del"
                                           :disable   (.isNull (ObjectExpression/objectExpression (.. selected-rels-widget getSelectionModel selectedItemProperty)))
                                           :on-action
                                                 (fn [e] (.removeAll joined-relations (.. selected-rels-widget getSelectionModel getSelectedItems)))})))
        widget (fx/v-box {}
                         add-widget
                         selected-rels-widget
                         )
        redraw-column-fn (fn []
                         (.setCellFactory selected-rels-widget
                                          (reify Callback
                                                    (call [this list-view]
                                                      (TextFieldListCell. (proxy [StringConverter] []
                                                                            (fromString [s])
                                                                            (toString [[v t]]
                                                                              (str (core/relation-to-filename v) " - "
                                                                              t ": " (get (get (gui/to-imut relations-colnames) v) t)))))))))
        ]
    (gui/on-any-change relations-colnames
                       redraw-column-fn)
    (doseq [idx (range 0 (count joined-relations))]
      (added-fn idx))
    (.addListener joined-relations
                  (reify ListChangeListener
                    (onChanged [this change]
                      (while (.next change)
                        (when (.wasAdded change)
                          (doseq [^long idx (range (.getFrom change) (.getTo change))]
                            (added-fn idx)
                            ))
                        (when (.wasRemoved change)
                          (let [removed (.getRemoved change)]
                            (doseq [^long idx (range (.getFrom change) (+ (.getFrom change) (.getRemovedSize change)))]
                              (removed-fn idx (.get removed (- idx (.getFrom change))))
                              )))
                        ))))
    (.addListener joinable-relations
                  (reify ListChangeListener
                    (onChanged [this change]
                      (while (.next change)
                        (when (.wasRemoved change)
                          (let [removed (.getRemoved change)
                                sel (vec joined-relations)
                                rem-set (set removed)]
                            (.removeAll joined-relations ^Collection (vec (filter (fn [[rel term]]
                                                                        (rem-set rel)) sel)))
                            (.removeAll unselected-relations removed)))
                        (when (.wasAdded change)
                          (let [added (.getAddedSubList change)]
                            (.addAll unselected-relations added)))
                        ))))
    widget))

(defn models-format-settings-widget [target-relation target-relation-index joinable-relations joined-relations relations-colnames validation]
  (let [term-widget (gui/make-relation-term-select-widget target-relation target-relation-index relations-colnames validation)
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
               (.add (make-rel-term-list joinable-relations joined-relations relations-colnames validation) 1 1)
               )]
    grid))


(def fields [:ace-loc :target-term :command :kb-format :models-format])

;knowledgebase format: model, key

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support)
        user-input (atom false)
        on-update-fn (fn []
                       (when @user-input
                         (async/put! >ui-requests {:type     :modified-page})))

        kb-formats-list (gui/observable-list (list ace/knowledgebase-key ace/knowledgebase-models))
        kb-selected-format (SimpleStringProperty. ace/knowledgebase-key)
        kb-format-widget (gui/choice-box kb-formats-list kb-selected-format)
        joined-relations (gui/observable-list)
        relation-list (gui/observable-list)
        joinable-relations (gui/observable-list)
        relations-colnames (gui/observable-map)
        target-relation (SimpleObjectProperty.)
        command-property (SimpleStringProperty.)
        command-widget (gui/text-combo-box (gui/observable-list (sort ace/example-commands)) command-property)

        target-relation-index (SimpleObjectProperty. 0)
        models-format-widget (models-format-settings-widget target-relation target-relation-index joinable-relations joined-relations relations-colnames validation)


        properties-map {:ace-loc       (gui/make-executable-item-editor "Ace executable"
                                                                           (:project-dir project)
                                                                           validation
                                                                           (fn [s]
                                                                             (ace/check-ace-path (core/resolve-executable-loc (:project-dir project) s)))
                                                                        "Could not find ace executable in specified path"
                                                                           on-update-fn
                                                                        )
                        :target-term   (gui/->PropertyItemEditor
                                         (gui/make-relation-select-widget relation-list target-relation validation)
                                         "Target relation"
                                         (SimpleObjectProperty. {:relation target-relation :relation-list relation-list :relations-colnames relations-colnames}))

                        :command       (gui/->PropertyItemEditor command-widget "Mining command" command-property)
                        :kb-format     (gui/->PropertyItemEditor kb-format-widget "Knowledgebase format" kb-selected-format)
                        :models-format (gui/->PropertyItemEditor models-format-widget "Models format settings" (SimpleObjectProperty.
                                                                                                                 {:joinable-relations    joinable-relations
                                                                                                                  :joined-relations      joined-relations
                                                                                                                  :target-relation-index target-relation-index
                                                                                                                  }))}
        properties-list (gui/observable-list (map properties-map fields))
        widget (make-widget properties-list)]
    (.bind (.disableProperty models-format-widget) (.isNotEqualTo (ObjectExpression/objectExpression kb-selected-format) ace/knowledgebase-models))
    (gui/validate-control validation command-widget (fn [cmd]
                                                      (and cmd (not (.isEmpty cmd)))) "You must enter an ace command")
    (doseq [prop [target-relation command-property kb-selected-format]]
      (gui/on-changed prop
                      (fn [obs old new]
                        (on-update-fn))))
    (gui/on-changed target-relation
                    (fn [obs old new]
                      (when @user-input
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
    (->AceSettingsPage widget properties-map user-input)))