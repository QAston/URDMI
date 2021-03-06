(ns urdmi.plugin.aleph.hypothesis-page
  (:require [fx-clj.core :as fx]
            [urdmi.gui-util :as gui]
            [clojure.core.async :as async]
            [urdmi.core :as core]
            [clojure.set :as set]
            [urdmi.plugin.aleph.core :as aleph]
            [urdmi.util :as util])
  (:import (javafx.scene.control TableView TreeTableView TreeTableColumn TreeItem TreeTableColumn$CellDataFeatures TableColumn TableView$TableViewSelectionModel SelectionMode Dialog ButtonType CheckBox)
           (javafx.scene.layout GridPane ColumnConstraints Region HBox Priority)
           (javafx.geometry Pos HPos)
           (javafx.util Callback StringConverter)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty SimpleLongProperty SimpleIntegerProperty SimpleBooleanProperty)
           (javafx.beans.binding ObjectExpression)
           (org.controlsfx.control PropertySheet PropertySheet$Mode)
           (javafx.scene.input KeyCode KeyCodeCombination KeyCombination$Modifier)
           (javafx.collections ListChangeListener ObservableList ObservableMap MapChangeListener MapChangeListener$Change)
           (javafx.application Platform)
           (javafx.event EventHandler)
           (java.util Map HashMap)
           (javafx.collections.transformation SortedList)))

;clauses are generated with single input variable for all possible input variable fields
;so that tables can be joined based on each kind of key
(defn new-relation-spec [[name arity :as relation] relation-columns]
  (let [this-rel-columns (get relation-columns relation (core/default-relation-column-description arity))
        default-spec (vec (map (fn [{:keys [name key]}] {:type "-" :value name})  this-rel-columns))
        term-specs (keep-indexed (fn [index {:keys [name key]}]
                                  (when (not= key :none)
                                    (assoc default-spec index {:type "+" :value name})))
                                 this-rel-columns)
        term-specs (if (empty? term-specs)
                     [default-spec]
                     term-specs)]

    (for [term-spec term-specs]
      {:relation    relation
       :determinacy "1"
       :terms       term-spec})))

(defn generate-relation-specs [relation-columns]
  (apply concat
    (for [relation (keys relation-columns)]
     (new-relation-spec relation relation-columns))))

(def m {"+" "+(input)"
        "-" "-(output)"
        "#" "#(constant)"
        ""  "(other)"})

(def morder ["+" "-" "#" ""])

(def default-relations aleph/available-modeh-clauses)

(defn make-term-spec-editor-widget [prop]
  (let [type-prop (SimpleStringProperty. (:type (.getValue prop)))
        value-prop (SimpleStringProperty. (:value (.getValue prop)))
        text-field (fx/text-field {:text (.getValue value-prop)})]
    (gui/on-changed type-prop
                    (fn [obs old new]
                      (.setValue prop {:type new :value (:value (.getValue prop))})))
    (gui/on-changed value-prop
                    (fn [obs old new]
                      (.setValue prop {:type (:type (.getValue prop)) :value new})))
    (gui/loose-bind (.textProperty text-field) value-prop)
    (fx/h-box {}
              (doto (gui/choice-box (gui/observable-list morder) type-prop)
                (.setConverter (proxy
                                 [StringConverter] []
                                 (toString [obj]
                                   (m obj)
                                   ))))
              text-field)

    ))


(defn show-clause-spec-editor [rel-spec]
  (let [rel-name (gui/->PropertyItem "Relation name/arity"
                                     ""
                                     String
                                     (SimpleObjectProperty. (core/relation-to-string (:relation rel-spec)))
                                     (fn [])
                                     false)

        determinacy-suggestions (gui/observable-list ["1" "*"])

        determinacy (let [prop (SimpleStringProperty. (:determinacy rel-spec))]
                      (gui/->PropertyItemEditor (gui/text-combo-box determinacy-suggestions prop) "Determinacy" prop))
        terms (for [[i term-spec] (map-indexed vector (:terms rel-spec))]
                (let [prop (SimpleObjectProperty. term-spec)]
                  (gui/->PropertyItemEditor (make-term-spec-editor-widget prop) (str "Term" i) prop)))

        editor (doto (PropertySheet. (gui/observable-list (into [rel-name determinacy] terms)))
                 (.setModeSwitcherVisible false)
                 (.setSearchBoxVisible false)
                 (.setMode PropertySheet$Mode/NAME)
                 (.setPropertyEditorFactory gui/property-editor-factory)
                 )

        dialog (doto (Dialog.)
                 (.setTitle "Edit clause spec")
                 (.. getDialogPane (setContent editor))
                 (.setWidth 400.0)
                 (.setHeight 600.0)
                 (.. getDialogPane getButtonTypes (setAll [ButtonType/OK, ButtonType/CANCEL]))
                 (.setResultConverter (reify Callback
                                        (call [this param]
                                          (if (= param ButtonType/OK)
                                            (-> rel-spec
                                                (assoc :determinacy (.getValue determinacy))
                                                (assoc :terms (mapv (memfn getValue) terms)))
                                            rel-spec
                                            )))))]
    (.orElse (.showAndWait dialog) nil)))

(defn make-clause-specs-table-entry [available-relations relation-columns clause-specs validation-support]
  (let [new-rel-spec (SimpleObjectProperty. nil)]
    (fx/h-box {:spacing 10.0}
              (doto (gui/make-relation-select-widget (SortedList. available-relations compare) new-rel-spec nil)
                (.setMaxWidth Double/MAX_VALUE)
                (HBox/setHgrow Priority/ALWAYS))
              (doto (fx/button {:text       "Add"
                                :pref-width 100.0
                                :disable    (.isNull (ObjectExpression/objectExpression new-rel-spec))
                                :on-action  (fn [e]
                                              (.addAll clause-specs (new-relation-spec (.getValue new-rel-spec) relation-columns)))})
                (HBox/setHgrow Priority/NEVER))
              (doto (fx/button {:text       "Add all"
                                :pref-width 120.0
                                :on-action  (fn [e]
                                              (.addAll clause-specs (generate-relation-specs relation-columns)))})
                (HBox/setHgrow Priority/NEVER))
              )))

(defn new-terms-column []
  (doto (TableColumn. "Terms")
    (.. getColumns
        (setAll []))))

(defn make-clause-specs-table [available-relations relation-columns validation-support specs-list]
  (let [terms-count (SimpleIntegerProperty. 0)

        new-clause (make-clause-specs-table-entry available-relations relation-columns specs-list validation-support)


        table-view (doto (TableView.)
                     (.setMinHeight 500.0)
                     (.. getColumns
                         (setAll [(doto (TableColumn. "RelationName/Arity")
                                    (.setPrefWidth 150.0)
                                    (.setCellValueFactory (reify Callback
                                                            (call [this data]
                                                              (let [row (.getValue data)]
                                                                (SimpleStringProperty.
                                                                  (core/relation-to-string (:relation row)))
                                                                ))))
                                    )
                                  (doto (TableColumn. "Determinacy")
                                    (.setPrefWidth 100.0)
                                    (.setCellValueFactory (reify Callback
                                                            (call [this data]
                                                              (let [row (.getValue data)]
                                                                (SimpleStringProperty.
                                                                  (:determinacy row))
                                                                )))))
                                  (new-terms-column)
                                  ])
                         )
                     (.. getSelectionModel (setSelectionMode SelectionMode/MULTIPLE)))

        update-spec-entry (fn []
                            (let [idx (.getFocusedIndex (.getSelectionModel table-view))
                                  new-spec (show-clause-spec-editor (.get specs-list idx))]
                              (when (not= new-spec (.get specs-list idx))
                                (.set ^ObservableList specs-list idx new-spec))
                              ))

        context-menu (doto (fx/context-menu)
                       (.. getItems (setAll [(fx/menu-item {:text        "Edit"
                                                            :accelerator (KeyCodeCombination. KeyCode/ENTER (make-array KeyCombination$Modifier 0))
                                                            :on-action   (fn [e]
                                                                           (when (not= -1 (.getFocusedIndex (.getSelectionModel table-view)))
                                                                             (update-spec-entry)))})
                                             (fx/menu-item {:text        "Remove"
                                                            :accelerator (KeyCodeCombination. KeyCode/DELETE (make-array KeyCombination$Modifier 0))
                                                            :on-action   (fn [e]
                                                                           (when-not (.isEmpty (.getSelectedItems (.getSelectionModel table-view)))
                                                                             (doseq [idx (reverse (sort (.getSelectedIndices (.getSelectionModel table-view))))]
                                                                               (.remove ^ObservableList specs-list ^int (int idx)))))})
                                             ])))
        ]
    (.addListener available-relations
                  (reify ListChangeListener
                    (onChanged [this change]
                      (while (.next change)
                        (let [rem-set (set (.getRemoved change))]
                          (when (.wasRemoved change)
                            (.removeIf ^ObservableList specs-list (util/predicate #(rem-set (:relation %)))
                                       )))))))
    (.addListener specs-list
                  (reify ListChangeListener
                    (onChanged [this change]
                      (while (.next change))
                      (.setValue terms-count (apply max 0
                                                    (map (fn [rel-spec]
                                                           (second (:relation rel-spec))) specs-list))))))
    (.setOnMouseClicked table-view (reify EventHandler
                                     (handle [this e]
                                       (when (and (not= -1 (.getFocusedIndex (.getSelectionModel table-view)))
                                                  (< 1 (.getClickCount e))
                                                  (update-spec-entry))
                                         ))))
    (gui/on-changed terms-count (fn [obs old new]
                                  (.remove (.getColumns table-view) 2)
                                  (let [terms-column (new-terms-column)]
                                    (gui/resize-observable-list (.getColumns terms-column) new
                                                                (fn [index]
                                                                  (doto (TableColumn. (str index))
                                                                    (.setCellValueFactory (reify Callback
                                                                                            (call [this data]
                                                                                              (let [row (.getValue data)]
                                                                                                (SimpleStringProperty.
                                                                                                  (if (< index (count (:terms row)))
                                                                                                    (aleph/term-spec-to-string ((:terms row) index))
                                                                                                    "")
                                                                                                  )))))
                                                                    (.setPrefWidth 100.0))))
                                    (.add (.getColumns table-view) terms-column))
                                  ))
    (.setContextMenu table-view context-menu)
    (.setItems table-view specs-list)
    (gui/border-wrap
      (fx/v-box {:spacing 5.0}
                new-clause
                table-view)
      "Clause specification")
    ))

(defn new-head-tree-item [rel]
  (doto (TreeItem. rel)
    (.setExpanded true)))

(defn new-body-tree-item [rel]
  (TreeItem. rel))

(defn make-hypothesis-table-entry [unique-relation-spec-names head-body-clauses validation-support]
  (let [new-hypothesis-head (SimpleObjectProperty. nil)
        new-hypothesis-body (SimpleObjectProperty. nil)
        available-body-rels (gui/observable-list)
        recalc-available-body-res (fn []
                                    (let [possible (set unique-relation-spec-names)
                                          selected (set (.get head-body-clauses (.getValue new-hypothesis-head)))
                                          target (set/difference possible selected)]
                                      (gui/sync-list available-body-rels target)))]
    (gui/on-changed new-hypothesis-head
                    (fn [obs old new]
                      (recalc-available-body-res)))
    (.addListener head-body-clauses (reify MapChangeListener
                                      (onChanged [this map-change]
                                        (let [^MapChangeListener$Change map-change map-change]
                                          (recalc-available-body-res)
                                          (when (.wasAdded map-change)
                                            (.addListener (.getValueAdded map-change) (reify ListChangeListener
                                                                                        (onChanged [this change]
                                                                                          (recalc-available-body-res)))))
                                          ))))
    (fx/h-box {:spacing 10.0}
              (doto (gui/make-relation-select-widget unique-relation-spec-names new-hypothesis-head nil)
                (.setMaxWidth Double/MAX_VALUE)
                (HBox/setHgrow Priority/ALWAYS))
              (doto (gui/make-relation-select-widget available-body-rels new-hypothesis-body nil)
                (.setMaxWidth Double/MAX_VALUE)
                (HBox/setHgrow Priority/ALWAYS))
              (doto (fx/button {:text       "Add"
                                :pref-width 70.0
                                :disable    (.or (.isNull (ObjectExpression/objectExpression new-hypothesis-head))
                                                 (.isNull (ObjectExpression/objectExpression new-hypothesis-body)))
                                :on-action  (fn [e]
                                              (if (.containsKey head-body-clauses (.getValue new-hypothesis-head))
                                                (.add (.get head-body-clauses (.getValue new-hypothesis-head)) (.getValue new-hypothesis-body))
                                                (.put ^Map head-body-clauses
                                                      (.getValue new-hypothesis-head)
                                                      (gui/observable-list [(.getValue new-hypothesis-body)]))))})
                (HBox/setHgrow Priority/NEVER))
              (doto (fx/button {:text       "Add all"
                                :pref-width 100.0
                                :disable    (.isNull (ObjectExpression/objectExpression new-hypothesis-head))
                                :on-action  (fn [e]
                                              (.remove head-body-clauses (.getValue new-hypothesis-head))
                                              (.put ^Map head-body-clauses
                                                    (.getValue new-hypothesis-head)
                                                    (gui/observable-list available-body-rels)))})
                (HBox/setHgrow Priority/NEVER))
              )))

(defn make-clause-hypothesis-view [validation-support head-body-clauses unique-relation-spec-names generate-head-body-clauses]
  (let [new-clause (make-hypothesis-table-entry unique-relation-spec-names head-body-clauses validation-support)

        tree-items (doto (TreeItem. "Root")
                     (.setExpanded true))

        tree-table-view (doto (TreeTableView.)
                          (.setMinHeight 300.0)
                          (.. getColumns
                              (setAll [(doto (TreeTableColumn. "Head")
                                         (.setPrefWidth 200.0)
                                         (.setCellValueFactory (reify Callback
                                                                 (call [this value]
                                                                   (let [^TreeTableColumn$CellDataFeatures value value
                                                                         show (identical? tree-items (.getParent (.getValue value)))]
                                                                     (SimpleStringProperty.
                                                                       (if show
                                                                         (core/relation-to-string (.getValue (.getValue value)))
                                                                         "")))
                                                                   ))))
                                       (doto (TreeTableColumn. "Body")
                                         (.setPrefWidth 250.0)
                                         (.setCellValueFactory (reify Callback
                                                                 (call [this value]
                                                                   (let [^TreeTableColumn$CellDataFeatures value value
                                                                         show (not (identical? tree-items (.getParent (.getValue value))))]
                                                                     (SimpleStringProperty.
                                                                       (if show
                                                                         (core/relation-to-string (.getValue (.getValue value)))
                                                                         "")))
                                                                   ))))
                                       ])))

        value-to-tree-item (HashMap.)

        context-menu (doto (fx/context-menu)
                       (.. getItems (setAll [(fx/menu-item {:text        "Remove"
                                                            :accelerator (KeyCodeCombination. KeyCode/DELETE (make-array KeyCombination$Modifier 0))
                                                            :on-action   (fn [e]
                                                                           (when-not (.isEmpty (.getSelectedItems (.getSelectionModel tree-table-view)))
                                                                             (doseq [^TreeItem tree-item (.getSelectedItems (.getSelectionModel tree-table-view))]
                                                                               (let [is-body (not (identical? tree-items (.getParent tree-item)))]
                                                                                 (if is-body
                                                                                   (if (< 1 (count (.get head-body-clauses (.getValue (.getParent tree-item)))))
                                                                                     (.remove ^ObservableList (.get head-body-clauses (.getValue (.getParent tree-item))) (.getValue tree-item))
                                                                                     (.remove head-body-clauses (.getValue (.getParent tree-item))))
                                                                                   (.remove head-body-clauses (.getValue tree-item))
                                                                                   ))
                                                                               )))})
                                             ])))
        check-box ^CheckBox (fx/check-box {:text "Generate from datamining learning example (default)"})
        ]
    ;won't work shitty framework
    #_(gui/validate-control validation-support check-box (fn [val]
                                                           (when val
                                                             (not-empty unique-relation-spec-names)
                                                             ))
                            "There must be clauses specified for autogeneration to occur")
    (.setContextMenu tree-table-view context-menu)
    (.setRoot tree-table-view tree-items)
    (.setShowRoot tree-table-view false)
    (.addListener head-body-clauses (reify MapChangeListener
                                      (onChanged [this map-change]
                                        (let [^MapChangeListener$Change map-change map-change]
                                          (when (.wasAdded map-change)
                                            (let [head-item (new-head-tree-item (.getKey map-change))]
                                              (.add (.getChildren tree-items) head-item)
                                              (.put value-to-tree-item (.getKey map-change) head-item)
                                              (doseq [body-item (.getValueAdded map-change)]
                                                (.add (.getChildren head-item) (new-body-tree-item body-item)))
                                              (.addListener ^ObservableList (.getValueAdded map-change)
                                                            (reify ListChangeListener
                                                              (onChanged [this change]
                                                                (while (.next change)
                                                                  (when (.wasRemoved change)
                                                                    (let [children (.getChildren head-item)]
                                                                      (doseq [idx (reverse (range (.getFrom change) (+ (.getFrom change) (.getRemovedSize change))))]
                                                                        (.remove ^ObservableList children (int idx))
                                                                        ))
                                                                    )
                                                                  (when (.wasAdded change)
                                                                    (doseq [idx (range (.getFrom change) (.getTo change))]
                                                                      (.add ^ObservableList (.getChildren head-item) (int idx) (new-body-tree-item (.get (.getList change) idx)))
                                                                      ))))))))
                                          (when (.wasRemoved map-change)
                                            (let [tree-item (.get value-to-tree-item (.getKey map-change))]
                                              (.remove value-to-tree-item (.getKey map-change))
                                              (.remove ^ObservableList (.getChildren tree-items) tree-item)))))))
    (.bindBidirectional (.selectedProperty check-box) generate-head-body-clauses)
    (.bind (.disableProperty new-clause) (.selectedProperty check-box))
    (.bind (.disableProperty tree-table-view) (.selectedProperty check-box))
    (gui/border-wrap
      (fx/v-box {:spacing 5.0}
                check-box
                new-clause
                tree-table-view)
      "Hypothesised clauses")
    ))

(defn get-hypothesis-settings [clause-settings hypothesis-settings]
  {:clause     (gui/map-of-mut-to-map-of-imut clause-settings)
   :hypothesis (let [{:keys [hypothesis-list autogenerate-hypothesis]} hypothesis-settings]
                 {:autogenerate-hypothesis (gui/to-imut autogenerate-hypothesis)
                  :hypothesis-list         (into {} (for [[k v] hypothesis-list]
                                                      [k (gui/to-imut v)]))})})

(defn sync-unique-rel-spec-names [clause-settings hypothesis-settings dependencies]
  (let [{:keys [all-relations datamining-settings unique-relation-spec-names]} dependencies]
    (when-let [datamining-settings (.getValue datamining-settings)]
      (gui/sync-list unique-relation-spec-names (set (map :relation (aleph/get-modeh-settings datamining-settings (get-hypothesis-settings clause-settings hypothesis-settings) all-relations)))))))

(defn sync-generated-hypothesis [clause-settings hypothesis-settings dependencies]
  (let [{:keys [all-relations datamining-settings]} dependencies
        {:keys [hypothesis-list]} hypothesis-settings]
    (when-let [datamining-settings (.getValue datamining-settings)]
      (let [hypo-list (aleph/get-hypothesis-list datamining-settings (get-hypothesis-settings clause-settings hypothesis-settings) all-relations)]
        (gui/from-imut hypothesis-list (into {} (for [[k v] hypo-list]
                                                  [k (gui/observable-list v)])))))))

(deftype HypothesisPage [widget clause-settings hypothesis-settings dependencies user-input]
  core/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (reset! user-input false)

    (let [{:keys [all-relations datamining-settings relation-columns]} dependencies]
      (gui/sync-list all-relations (concat (core/get-all-relation-names project) default-relations))
      (.setValue datamining-settings (core/get-settings-data project aleph/datamining-name))
      (gui/from-imut relation-columns (core/generate-relation-column-map project))
      )
    (when modified
      (let [data (core/get-settings-data project (last key))]
        (gui/map-of-mut-from-map-of-imut clause-settings (:clause data))
        (let [{:keys [hypothesis-list autogenerate-hypothesis]} hypothesis-settings]
          (gui/from-imut autogenerate-hypothesis (:autogenerate-hypothesis (:hypothesis data)))
          (gui/from-imut hypothesis-list (into {} (for [[k v] (:hypothesis-list (:hypothesis data))]
                                                    [k (gui/observable-list v)]))))
        ))
    ;sync modified
    (sync-unique-rel-spec-names clause-settings hypothesis-settings dependencies)
    (sync-generated-hypothesis clause-settings hypothesis-settings dependencies)
    (reset! user-input true)
    )
  (read-data [this]
    (core/file-item (get-hypothesis-settings clause-settings hypothesis-settings))
    ))

(defn make-widget [{specs-list :clause-list :as clause-settings}
                   {head-body-clauses :hypothesis-list generate-head-body-clauses :autogenerate-hypothesis :as hypothesis-settings}
                   {available-relations :all-relations unique-relation-spec-names :unique-relation-spec-names relation-columns :relation-columns :as dependencies} validation-support]
  (gui/on-any-change specs-list
                     (fn []
                       (sync-unique-rel-spec-names clause-settings hypothesis-settings dependencies)))
  (gui/on-any-change unique-relation-spec-names
                     (fn []
                       (sync-generated-hypothesis clause-settings hypothesis-settings dependencies)))
  (gui/on-changed generate-head-body-clauses (fn [obs old new]
                                               (when (= new true)
                                                 (sync-generated-hypothesis clause-settings hypothesis-settings dependencies))))
  (fx/v-box {}
            (make-clause-specs-table available-relations relation-columns validation-support specs-list)
            (make-clause-hypothesis-view validation-support head-body-clauses unique-relation-spec-names generate-head-body-clauses)
            ))

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support)
        clause-settings {:clause-list (gui/observable-list)}
        hypothesis-settings {:hypothesis-list         (gui/observable-map)
                             :autogenerate-hypothesis (SimpleBooleanProperty. true)}
        dependencies {:all-relations              (gui/observable-list)
                      :unique-relation-spec-names (gui/observable-list)
                      :datamining-settings        (SimpleObjectProperty.)
                      :relation-columns           (gui/observable-map)}
        user-input (atom nil)
        on-update-fn (fn []
                       (when @user-input
                         (async/put! >ui-requests {:type :modified-page})))
        widget (make-widget clause-settings hypothesis-settings dependencies validation)]
    (gui/map-of-mut-on-any-change clause-settings on-update-fn)
    (gui/map-of-mut-on-any-change hypothesis-settings on-update-fn)
    (->HypothesisPage widget clause-settings hypothesis-settings dependencies user-input)))