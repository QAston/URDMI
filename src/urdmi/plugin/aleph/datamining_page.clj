(ns urdmi.plugin.aleph.datamining-page
  (:require [urdmi.gui :as gui]
            [clojure.core.async :as async]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [urdmi.plugin.aleph.core :as aleph])
  (:import (javafx.event EventHandler)
           (javafx.scene.control TableView TableColumn SelectionMode ToggleButton ToggleGroup)
           (javafx.beans.property SimpleStringProperty SimpleObjectProperty)
           (javafx.util Callback StringConverter)
           (javafx.scene.input KeyCodeCombination KeyCombination$Modifier KeyCode)
           (javafx.collections ObservableList)
           (javafx.geometry Insets HPos Pos)
           (org.controlsfx.control ListSelectionView SegmentedButton)
           (javafx.scene.control.cell TextFieldListCell)
           (javafx.scene.layout GridPane ColumnConstraints)
           (org.controlsfx.tools Borders)
           (javafx.application Platform)))

(def type-str {:positive "Positive"
               :negative "Negative"})

(defn make-simple-example-data-widget []
  (let [validation-support (gui/validation-support)
        relations-list (gui/observable-list [["asd" 1] ["wqe" 3]])
        new-relation (SimpleObjectProperty. nil)
        new-relation-term (SimpleObjectProperty. nil)
        relations-to-term-values (gui/observable-map {["asd" 1] [#{"0" "1"}]})
        current-term-values (gui/observable-list)
        new-positive-value (SimpleStringProperty. nil)
        new-negative-value (SimpleStringProperty. nil)
        pos-value-select-widget (gui/choice-box current-term-values new-positive-value)
        neg-value-select-widget (gui/choice-box current-term-values new-negative-value)

        ]
    (fx/h-box {:spacing   5.0
               :alignment Pos/CENTER_LEFT}
              (fx/label {:text "Relation term"})
              (gui/make-relation-select-widget relations-list new-relation validation-support)
              (gui/make-relation-term-select-widget new-relation new-relation-term validation-support)
              (fx/label {:text "Positive value"})
              pos-value-select-widget
              (fx/label {:text "Negative value"})
              neg-value-select-widget
              )
    ))

(defn make-new-advanced-example-data-widget [advanced-examples-list table-cols]
  (let [validation-support (gui/validation-support)
        relations-list (gui/observable-list [["asd" 1] ["wqe" 3]])
        new-relation (SimpleObjectProperty. nil)
        new-relation-term (SimpleObjectProperty. nil)
        relations-to-term-values (gui/observable-map {["asd" 1] [#{"0" "1"}]})
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
        rel-select-widget (gui/make-relation-select-widget relations-list new-relation validation-support)
        rel-term-select-widget (gui/make-relation-term-select-widget new-relation new-relation-term validation-support)

        ]
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

(defn make-example-data-widget []
  (let [advanced-examples-list (gui/observable-list)

        table-view (doto (TableView.)
                     (.setMinHeight 140.0)
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
                                                                  (:relation-term row))
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

        new-advanced-entry-widget (make-new-advanced-example-data-widget advanced-examples-list (.getColumns table-view))

        context-menu (doto (fx/context-menu)
                       (.. getItems (setAll [(fx/menu-item {:text        "Remove"
                                                            :accelerator (KeyCodeCombination. KeyCode/DELETE (make-array KeyCombination$Modifier 0))
                                                            :on-action   (fn [e]
                                                                           (when-not (.isEmpty (.getSelectedItems (.getSelectionModel table-view)))
                                                                             (doseq [idx (reverse (sort (.getSelectedIndices (.getSelectionModel table-view))))]
                                                                               (.remove ^ObservableList advanced-examples-list ^int (int idx)))))})
                                             ])))

        simple-widget (make-simple-example-data-widget)

        advanced-widget (fx/v-box {}
                                  new-advanced-entry-widget
                                  table-view)

        simple-advanced-selection (SimpleObjectProperty. nil)

        cb-simple (fx/radio-button {:text "Simple (default)"})
        cb-advanced (fx/radio-button {:text "Advanced"})
        cb-group (ToggleGroup.)
        vbox (fx/v-box {:spacing 5.0}
                       cb-simple
                       cb-advanced)

        grid (.. (Borders/wrap
                   vbox)
                 (lineBorder)
                 (title "Select learning example data")
                 (build)
                 (build))]
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

(defn make-select-program-widget []
  (fx/h-box {:spacing   5.0
             :alignment Pos/CENTER_LEFT
             :padding   (Insets. 0 0 5.0 10.0)}
            (fx/label {:text "Datamining program"})
            (gui/choice-box (gui/observable-list aleph/programs) (SimpleObjectProperty.))))

(defn make-background-data-widget []
  (let [available-relations (gui/observable-list [["asd" 1] ["we" 2]])
        selected-relations (gui/observable-list)
        list (doto (ListSelectionView.)
               (.setSourceHeader (fx/label {:text "Available relations"}))
               (.setTargetHeader (fx/label {:text "Included in background knowledge"}))
               (.setSourceItems available-relations)
               (.setTargetItems selected-relations)
               (.setCellFactory (reify Callback
                                  (call [this list-view]
                                    (TextFieldListCell. (proxy [StringConverter] []
                                                          (fromString [s])
                                                          (toString [v]
                                                            (core/relation-to-string v)
                                                            )))))))
        cb-all (fx/radio-button {:text "Include all"})
        cb-all-but-example (fx/radio-button {:text "Include all but examples (default)"})
        cb-selected (fx/radio-button {:text "Include selected"})
        include-setting (SimpleObjectProperty.)
        cb-group (ToggleGroup.)
        ]
    (.. cb-group getToggles (setAll [cb-all-but-example cb-all cb-selected]))
    (bidirectional-bind-toggle-to-property cb-group [:all-but-example :all :selected] include-setting)
    (gui/on-changed include-setting (fn [obs old new]
                                      (.setDisable list (not (= new :selected)))))
    (.setValue include-setting :all-but-example)
    (.. (Borders/wrap
          (fx/v-box {}
                    cb-all-but-example
                    cb-all
                    cb-selected
                    list))
        (lineBorder)
        (title "Select background relations")
        (build)
        (build)))

  )

(deftype DataminingPage [widget data user-input]
  gui/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (reset! user-input false)
    (if modified
      nil
      nil)
    (reset! user-input true)
    )
  (read-data [this]))

(defn make-widget []
  (fx/v-box {}
            (make-select-program-widget)
            (make-example-data-widget)
            (make-background-data-widget)))

(defn make-page [>ui-requests project]
  (let [validation (gui/validation-support)
        user-input (atom nil)
        on-update-fn (fn []
                       (when @user-input
                         (async/put! >ui-requests {:type :modified-page})))
        widget (make-widget)]
    (->DataminingPage widget nil user-input)))