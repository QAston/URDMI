(ns urdmi.plugin.aleph.datamining-page
  (:require [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [urdmi.plugin.aleph.core :as aleph]
            [clojure.set :as set])
  (:import (javafx.event EventHandler)
           (javafx.scene.control TableView TableColumn SelectionMode ToggleButton ToggleGroup)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty)
           (javafx.util Callback StringConverter)
           (javafx.scene.input KeyCodeCombination KeyCombination$Modifier KeyCode)
           (javafx.collections ObservableList MapChangeListener MapChangeListener$Change ListChangeListener)
           (javafx.geometry Insets HPos Pos)
           (org.controlsfx.control ListSelectionView SegmentedButton)
           (javafx.scene.control.cell TextFieldListCell)
           (java.util Collection)))

(def type-str {:positive "Positive"
               :negative "Negative"})

(def program-string {"custom" "custom (Advanced)"
                     "induce" "induce (Default)"
                     })

(defn make-simple-example-data-widget [new-relation
                                       new-relation-term
                                       new-positive-value
                                       new-negative-value
                                       all-relations
                                       relations-to-term-values
                                       validation-support]
  (let [current-term-values (gui/observable-list)
        pos-value-select-widget (gui/choice-box current-term-values new-positive-value)
        neg-value-select-widget (gui/choice-box current-term-values new-negative-value)

        recalc-available-term-values (fn []
                                       (let [new-term-vals (if (and (.getValue new-relation) (.getValue new-relation-term))
                                                             (get-in relations-to-term-values [(.getValue new-relation) (.getValue new-relation-term)])
                                                             [])]
                                         (gui/sync-list current-term-values new-term-vals)))
        ]
    (gui/on-any-change new-relation recalc-available-term-values)
    (gui/on-any-change new-relation-term recalc-available-term-values)
    (gui/on-any-change relations-to-term-values recalc-available-term-values)
    (gui/validate-control validation-support
                          pos-value-select-widget
                      (fn [val]
                        (boolean val)) "You must select a value of term by which positive examples will be selected")

    (gui/validate-control validation-support
                          neg-value-select-widget
                          (fn [val]
                            (boolean val)) "You must select a value of term by which negative examples will be selected")

    (fx/h-box {:spacing   5.0
               :alignment Pos/CENTER_LEFT}
              (fx/label {:text "Relation term"})
              (gui/make-relation-select-widget all-relations new-relation validation-support)
              (gui/make-relation-term-select-widget new-relation new-relation-term validation-support)
              (fx/label {:text "Positive value"})
              pos-value-select-widget
              (fx/label {:text "Negative value"})
              neg-value-select-widget
              )
    ))

(defn make-new-advanced-example-data-widget [advanced-examples-list table-cols relations-list relations-to-term-values validation-support]
  (let [new-relation (SimpleObjectProperty. nil)
        new-relation-term (SimpleObjectProperty. nil)
        current-term-values (gui/observable-list)
        new-value-type (SimpleObjectProperty. nil)
        new-value (SimpleStringProperty. nil)
        value-type-select-widget (doto (gui/choice-box (gui/observable-list [:positive :negative]) new-value-type)
                                   (.setConverter (proxy
                                                    [StringConverter] []
                                                    (toString [obj]
                                                      (type-str obj)
                                                      ))))
        value-select-widget (gui/choice-box current-term-values new-value)
        rel-select-widget (gui/make-relation-select-widget relations-list new-relation nil)
        rel-term-select-widget (gui/make-relation-term-select-widget new-relation new-relation-term nil)

        recalc-available-term-values (fn []
                                       (let [new-term-vals (if (and (.getValue new-relation) (.getValue new-relation-term))
                                                             (get-in relations-to-term-values [(.getValue new-relation) (.getValue new-relation-term)])
                                                             [])]
                                         (gui/sync-list current-term-values new-term-vals)))
        ]
    (gui/on-any-change new-relation recalc-available-term-values)
    (gui/on-any-change new-relation-term recalc-available-term-values)
    (gui/on-any-change relations-to-term-values recalc-available-term-values)

    (doseq [[widget col] (map vector [rel-select-widget rel-term-select-widget
                                      value-type-select-widget value-select-widget]
                              table-cols)]
      (.bind (.minWidthProperty widget) (.widthProperty col))
      (.bind (.maxWidthProperty widget) (.widthProperty col))
      (.setPadding widget (Insets. 0 1 0 1))
      )
    (fx/h-box {}
              rel-select-widget
              rel-term-select-widget
              value-type-select-widget
              value-select-widget
              (fx/button {:text      "Add"
                          :disable   (.or (.isNull new-value-type) (.or (.isNull new-value) (.or (.isNull new-relation) (.isNull new-relation-term))))
                          :on-action (fn [e]
                                       (.add advanced-examples-list {:value         (.getValue new-value)
                                                                     :value-type    (.getValue new-value-type)
                                                                     :relation      (.getValue new-relation)
                                                                     :relation-term (.getValue new-relation-term)}))}))
    ))

(defn bidirectional-bind-toggle-to-property [^ToggleGroup toggle-group toggle-vals ^SimpleObjectProperty property]
  (gui/on-changed (.selectedToggleProperty toggle-group)
                  (fn [obs old new]
                    (if new
                      (.setValue property ((zipmap (.getToggles toggle-group) toggle-vals) new))
                      (.setValue property nil))))
  (gui/on-changed property (fn [obs old new]
                             (if new
                               (.selectToggle toggle-group ((zipmap toggle-vals (.getToggles toggle-group)) new))
                               (.selectToggle toggle-group nil)))))

(defn make-example-data-widget [{simple-advanced-selection :type
                                 relation-selection        :relation
                                 term-selection            :term
                                 true-val-selection        :true-val
                                 false-val-selection       :false-val :keys [advanced-list]}
                                {:keys [all-relations relations-term-values]}
                                validation-support]
  (let [table-view (doto (TableView.)
                     (.setMinHeight 140.0)
                     (.setItems advanced-list)
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
                                  (doto (TableColumn. "Term")
                                    (.setPrefWidth 50.0)
                                    (.setCellValueFactory (reify Callback
                                                            (call [this data]
                                                              (let [row (.getValue data)]
                                                                (SimpleStringProperty.
                                                                  (str (:relation-term row)))
                                                                )))))
                                  (doto (TableColumn. "Pos/Neg")
                                    (.setPrefWidth 100.0)
                                    (.setCellValueFactory (reify Callback
                                                            (call [this data]
                                                              (let [row (.getValue data)]
                                                                (SimpleStringProperty.
                                                                  (type-str (:value-type row)))
                                                                )))))
                                  (doto (TableColumn. "Value")
                                    (.setPrefWidth 100.0)
                                    (.setCellValueFactory (reify Callback
                                                            (call [this data]
                                                              (let [row (.getValue data)]
                                                                (SimpleStringProperty.
                                                                  (:value row))
                                                                )))))
                                  ])
                         )
                     (.. getSelectionModel (setSelectionMode SelectionMode/MULTIPLE)))

        new-advanced-entry-widget (make-new-advanced-example-data-widget advanced-list (.getColumns table-view) all-relations relations-term-values validation-support)

        context-menu (doto (fx/context-menu)
                       (.. getItems (setAll [(fx/menu-item {:text        "Remove"
                                                            :accelerator (KeyCodeCombination. KeyCode/DELETE (make-array KeyCombination$Modifier 0))
                                                            :on-action   (fn [e]
                                                                           (when-not (.isEmpty (.getSelectedItems (.getSelectionModel table-view)))
                                                                             (doseq [idx (reverse (sort (.getSelectedIndices (.getSelectionModel table-view))))]
                                                                               (.remove ^ObservableList advanced-list ^int (int idx)))))})
                                             ])))

        simple-widget (make-simple-example-data-widget relation-selection
                                                       term-selection
                                                       true-val-selection
                                                       false-val-selection
                                                       all-relations
                                                       relations-term-values
                                                       validation-support)

        advanced-widget (fx/v-box {}
                                  new-advanced-entry-widget
                                  table-view)

        cb-simple (fx/radio-button {:text "Simple (default)"})
        cb-advanced (fx/radio-button {:text "Advanced"})
        cb-group (ToggleGroup.)
        vbox (fx/v-box {:spacing 5.0}
                       cb-simple
                       cb-advanced)

        grid (gui/border-wrap vbox "Select learning example data")]
    (.. cb-group getToggles (setAll [cb-simple cb-advanced]))
    (bidirectional-bind-toggle-to-property cb-group [:simple :advanced] simple-advanced-selection)
    (gui/on-changed simple-advanced-selection (fn [obs old new]
                                                (when old
                                                  (.remove ^ObservableList (.getChildren vbox) ^long (dec (count (.getChildren vbox)))))
                                                (when new
                                                  (.add (.getChildren vbox)
                                                        (if (= new :simple)
                                                          simple-widget
                                                          advanced-widget)))))
    (.setValue simple-advanced-selection :simple)
    (.setContextMenu table-view context-menu)

    grid
    ))

(defn make-select-program-widget [{:keys [program]} validation]
  (fx/h-box {:spacing   5.0
             :alignment Pos/CENTER_LEFT
             :padding   (Insets. 0 0 5.0 10.0)}
            (fx/label {:text "Datamining program"})
            (doto (gui/choice-box (gui/observable-list aleph/programs) program)
              (.setConverter (proxy
                               [StringConverter] []
                               (toString [obj]
                                 (if-let [s (program-string obj)]
                                   s
                                   obj)
                                 ))))))

(defn make-background-data-widget [{include-setting :type selected-relations :relation-list :as background-settings} example-settings {:keys [all-relations available-relations]} validation-support]
  (let [list (doto (ListSelectionView.)
               (.setSourceHeader (fx/label {:text "Available relations"}))
               (.setTargetHeader (fx/label {:text "Included in background knowledge"}))
               (.setSourceItems available-relations)
               (.setTargetItems selected-relations)
               (.setCellFactory (reify Callback
                                  (call [this list-view]
                                    (TextFieldListCell. gui/relation-string-converter)))))
        cb-all (fx/radio-button {:text "Include all"})
        cb-all-but-example (fx/radio-button {:text "Include all but examples (default)"})
        cb-selected (fx/radio-button {:text "Include selected"})
        cb-group (ToggleGroup.)
        ]

    (.. cb-group getToggles (setAll [cb-all-but-example cb-all cb-selected]))
    (bidirectional-bind-toggle-to-property cb-group [:all-but-example :all :selected] include-setting)
    (gui/on-changed include-setting (fn [obs old new]
                                      (.setDisable list (not (= new :selected)))
                                      (when (not (= new :selected))
                                        (if (= new :all)
                                          (gui/sync-list available-relations [])
                                          (gui/sync-list available-relations (set (map :relation (aleph/get-learning-examples-settings (gui/map-of-mut-to-map-of-imut example-settings))))))
                                        (.removeAll ^ObservableList selected-relations ^Collection (set/difference (set selected-relations) (set all-relations))))))
    (.setValue include-setting :all-but-example)
    (gui/border-wrap (fx/v-box {}
                               cb-all-but-example
                               cb-all
                               cb-selected
                               list)
                     "Select background relations")))

(defn get-datamining-settings [example-settings background-settings other-settings]
  (merge {:example    (gui/map-of-mut-to-map-of-imut example-settings)
                          :background (gui/map-of-mut-to-map-of-imut background-settings)}
                         (gui/map-of-mut-to-map-of-imut other-settings)))

(deftype DataminingPage [widget example-settings background-settings other-settings dependencies user-input]
  gui/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (reset! user-input false)
    (let [{:keys [all-relations relations-term-values]} dependencies]
      ;external mod handle
      (gui/sync-list all-relations (core/get-all-relation-names project))
      (.putAll relations-term-values (core/generate-relation-term-values-map project)))

      (when modified
        (let [data (core/get-settings-data project (last key))]
        (gui/map-of-mut-from-map-of-imut example-settings (:example data))
        (gui/map-of-mut-from-map-of-imut background-settings (:background data))
        (gui/map-of-mut-from-map-of-imut other-settings data))
        )
    ;external mod handle
      (gui/sync-list (:relation-list background-settings) (aleph/get-background-relations (get-datamining-settings example-settings background-settings other-settings) (:all-relations dependencies)))
      (gui/sync-list (:available-relations dependencies) (set/difference (set (:all-relations dependencies)) (set (:relation-list background-settings))))
    (reset! user-input true)
    )
  (read-data [this]
    (core/file-item (get-datamining-settings example-settings background-settings other-settings) )
    ))

(defn make-widget [example-settings background-settings other-settings dependencies validation]
  (fx/v-box {}
            (make-select-program-widget other-settings validation)
            (make-example-data-widget example-settings dependencies validation)
            (make-background-data-widget background-settings example-settings dependencies validation)))

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support)
        example-settings {:type          (SimpleObjectProperty.)
                          :relation      (SimpleObjectProperty.)
                          :term          (SimpleObjectProperty.)
                          :true-val      (SimpleStringProperty.)
                          :false-val     (SimpleStringProperty.)
                          :advanced-list (gui/observable-list)}
        background-settings {:type          (SimpleObjectProperty.)
                             :relation-list (gui/observable-list)}
        other-settings {:program (SimpleStringProperty.)}
        dependencies {:all-relations         (gui/observable-list)
                      :relations-term-values (gui/observable-map)
                      :available-relations   (gui/observable-list)}
        user-input (atom nil)
        on-update-fn (fn []
                       (when @user-input
                         (async/put! >ui-requests {:type :modified-page})))
        widget (make-widget example-settings background-settings other-settings dependencies validation)]
    (gui/map-of-mut-on-any-change example-settings on-update-fn)
    (gui/map-of-mut-on-any-change background-settings on-update-fn)
    (gui/map-of-mut-on-any-change other-settings on-update-fn)
    (->DataminingPage widget example-settings background-settings other-settings dependencies user-input)))