(ns urdmi.gui.relation
  (:require [clojure.core.async :refer [chan go <! >!]]
            [fx-clj.core :as fx]
            [urdmi.prolog :as prolog]
            [clojure.string :as string]
            [urdmi.util :as util])
  (:import
    (javafx.scene.layout Region VBox Priority HBox)
    (javafx.geometry Pos Insets)
    (javafx.scene.control TableColumn TableView SelectionMode ContextMenu MenuItem TablePosition TextField TableColumn$CellEditEvent)
    (javafx.collections ObservableList FXCollections)
    (java.util List)
    (javafx.util Callback)
    (javafx.scene.control.cell TextFieldTableCell)
    (java.io StringWriter)
    (javafx.beans.value ChangeListener)
    (javafx.beans.property StringProperty SimpleStringProperty SimpleLongProperty)
    (javafx.scene.input KeyCodeCombination KeyCode Clipboard ClipboardContent)
    (javafx.event EventHandler)
    (javafx.util.converter DefaultStringConverter)
    (org.apache.commons.lang3.reflect FieldUtils)))

(defn- rel-ast-to-table [rel-asts]
  (let [op-manager (:op-manager (prolog/parser-context []))
        ]
    (->> rel-asts
         (mapv (fn [ast]
                 (->> ast
                      :children
                      rest
                      (mapv (fn [ast]
                              (let [writer (StringWriter.)]
                                (prolog/pretty-print ast op-manager writer)
                                (.toString writer))))))))))

(defn generate-viewmodel [rel]
  (let [rel-asts (:ast rel)
        [rel-name rel-arity] (:rel rel)]
    {:name  rel-name
     :arity rel-arity
     :data  (rel-ast-to-table rel-asts)}
    ))

(defn- calc-cell-bounds [cells]
  (if (<= (count cells) 1)
    [(.getColumn (first cells)) (.getRow (first cells)) (.getColumn (first cells)) (.getRow (first cells))]
    (reduce (fn [tp-a ^TablePosition tp-b]
              (if (vector? tp-a)
                (let [[left top right bottom] tp-a]
                  [(min left (.getColumn tp-b))
                   (min top (.getRow tp-b))
                   (max right (.getColumn tp-b))
                   (max bottom (.getRow tp-b))])
                [(min (.getColumn tp-a) (.getColumn tp-b))
                 (min (.getRow tp-a) (.getRow tp-b))
                 (max (.getColumn tp-a) (.getColumn tp-b))
                 (max (.getRow tp-a) (.getRow tp-b))])
              )
            cells)))

(defn- copy-action [^TableView relation-table]
  (when-not (.. relation-table getSelectionModel getSelectedItems isEmpty)
    (let [selection-model (.getSelectionModel relation-table)
          sorted-cells (sort-by (fn [^TablePosition table-position]
                                  [(.getRow table-position) (.getColumn table-position)])
                                (seq (.getSelectedCells selection-model)))
          [left-bound top-bound right-bound bottom-bound] (calc-cell-bounds sorted-cells)

          data (loop [cells sorted-cells builder (StringBuilder.) row top-bound col left-bound]
                 (if-not (seq cells)
                   (.toString builder)
                   (let [^TablePosition next (first cells)]
                     (cond (< row (.getRow next))
                           (do
                             (dotimes [i (inc (- right-bound col))]
                               (.append builder \tab))
                             (.append builder "\n")
                             (recur cells builder (inc row) left-bound))
                           :else
                           (do
                             (dotimes [i (- (.getColumn next) col)]
                               (.append builder \tab))
                             (.append builder (.getValue ^StringProperty (.get ^List (.get (.getItems relation-table)
                                                                                           (.getRow next)) (.getColumn next))))
                             (when-not (= (.getColumn next) right-bound)
                               (.append builder \tab))
                             (recur (rest cells) builder row (inc (.getColumn next))))
                           ))))]
      data)))

(defn- paste-action [in-string ^TableView relation-table]
  (when (and in-string (not (.. relation-table getSelectionModel getSelectedItems isEmpty)))
    (let [data (for [line (string/split-lines in-string)]
                 (string/split line #"\t"))
          ^ObservableList items (.getItems relation-table)
          [left-bound top-bound right-bound bottom-bound] (calc-cell-bounds
                                                            (seq (.. relation-table
                                                                     getSelectionModel
                                                                     getSelectedCells)))]
      (doseq [[row-index src-row] (map-indexed util/args-vec data)
              :when (< (+ top-bound row-index) (count items))
              :let [^ObservableList target-row (.get items (+ top-bound row-index))]]
        (doseq [[col-index src-col] (map-indexed util/args-vec src-row)]
          (when (< (+ col-index left-bound) (count target-row))
            (.setValue ^StringProperty (.get target-row (+ col-index left-bound)) src-col))
          ))
      )))

(defn- delete-rows-action [^TableView relation-table]
  (let [items (seq (.. relation-table
                       getSelectionModel
                       getSelectedCells))
        rows (->> items
                  (map (fn [^TablePosition tp]
                         (.getRow tp)))
                  (sort)
                  (reverse)
                  (distinct)
                  )]
    (doseq [row-index rows]
      (.remove (.getItems relation-table) (int row-index)))
    ))

(defn- new-relation-row [arity]
  (FXCollections/observableArrayList (for [i (range arity)]
                                       (SimpleStringProperty. "0"))))

(defn- insert-row-action [^TableView relation-table action-event]
  (let [items (seq (.. relation-table
                       getSelectionModel
                       getSelectedCells))
        rows (->> items
                  (map (fn [^TablePosition tp]
                         (.getRow tp)))
                  (sort)
                  (reverse)
                  (distinct)
                  )]
    (doseq [row-index rows]
      (.add (.getItems relation-table)
            row-index
            (new-relation-row (count (.get (.getItems relation-table) (int row-index))))
            ))))

(defn- build-context-menu [^TableView relation-table]
  (doto (ContextMenu.)
    (.. getItems
        (add (doto (MenuItem. "Copy")
               (.setAccelerator (KeyCodeCombination. KeyCode/C (into-array (list KeyCodeCombination/SHORTCUT_DOWN))))
               (.setOnAction (reify EventHandler
                               (handle [this action-event]
                                 (.setContent (Clipboard/getSystemClipboard)
                                              (doto (ClipboardContent.)
                                                (.putString (copy-action relation-table))))
                                 ))))))
    (.. getItems
        (add (doto (MenuItem. "Paste")
               (.setAccelerator (KeyCodeCombination. KeyCode/V (into-array (list KeyCodeCombination/SHORTCUT_DOWN))))
               (.setOnAction (reify EventHandler
                               (handle [this action-event]
                                 (paste-action
                                   (.getString (Clipboard/getSystemClipboard))
                                   relation-table)))))))
    (.. getItems
        (add (doto (MenuItem. "Delete selected rows")
               (.setOnAction (reify EventHandler
                               (handle [this action-event]
                                 (delete-rows-action relation-table)))))))
    (.. getItems
        (add (doto (MenuItem. "Insert row above")
               (.setOnAction (reify EventHandler
                               (handle [this action-event]
                                 (insert-row-action relation-table action-event)))))))
    ))

(defn- build-edit-table [^TableView relation-table view-model]
  (let [context-menu (build-context-menu relation-table)]
    (.. relation-table
        getColumns
        (setAll (for [i (range (:arity view-model))]
                  (doto (TableColumn. (str "term_" i))
                    (.setEditable true)
                    (.setCellFactory (reify Callback
                                       (call [this table-column]
                                         (doto (proxy [TextFieldTableCell] [(DefaultStringConverter.)]
                                                 (startEdit []
                                                   (let [cell this
                                                         oldTextField (FieldUtils/readField this "textField", true)
                                                         _ (proxy-super startEdit)
                                                         ^TextField newTextField (FieldUtils/readField this "textField", true)]
                                                     (when-not (identical? oldTextField newTextField)
                                                       (.addListener (.focusedProperty newTextField)
                                                                     (reify ChangeListener
                                                                       (changed [this observale old new]
                                                                         (when-not new
                                                                           (.commitEdit ^TextFieldTableCell cell
                                                                                        (.getText newTextField))

                                                                           ;this is a hack, because isEditing is false
                                                                           ;when changing to another cell
                                                                           (let [col i
                                                                                 row (.getIndex cell)
                                                                                 cell-val (.get (.get (.getItems relation-table) row) col)
                                                                                 selected-cell-pos (first (.. relation-table
                                                                                                              getSelectionModel
                                                                                                              getSelectedCells))]
                                                                             (when-not (and selected-cell-pos (or (not= (.getColumn selected-cell-pos) col) (not= (.getRow selected-cell-pos) row))
                                                                                            (.setValue cell-val (.getText newTextField)))))))))))))
                                           (.setContextMenu context-menu)))
                                       ))
                    (.setOnEditCommit (reify EventHandler
                                        (handle [this cell-edit-event]
                                          (let [row (.getRowValue cell-edit-event)
                                                col-index (.. cell-edit-event getTablePosition getColumn)
                                                newValue (.getNewValue cell-edit-event)]
                                            (.setValue (.get row col-index) newValue)))))
                    (.setCellValueFactory (reify Callback
                                            (call [this cell-data-features]
                                              (.get (.getValue cell-data-features) i))
                                            ))))))
    ))

;a panel with a table view with remove add column buttons and sorting
;also rename relation textbox
;todo: have mutuable model, which is mapped to a persistent data structure for history support on change commit
;multiple selection: ctrl + click on cell to add cell to the selection
; shift + click, add all cells in bettween
(defn build-relation-edit-widget [view-model]
  (let [name-property (SimpleStringProperty. (:name view-model))
        arity-property (SimpleLongProperty. (:arity view-model))]
    (list (doto (fx/h-box {:padding   (Insets. 5 5 5 5)
                           :alignment (Pos/CENTER_LEFT)}
                          (fx/label {:padding (Insets. 3 3 3 3)} "Name:")
                          (let [text-field (fx/text-field {:padding (Insets. 3 3 3 3)} (.getValue name-property))]
                            (.addListener (.focusedProperty text-field)
                                          (reify ChangeListener
                                            (changed [this observable old new]
                                              (when-not new
                                                (.setValue name-property (Long/valueOf ^String (.getText text-field)))))))
                            text-field)
                          (fx/label {:padding (Insets. 3 3 3 13)} "Arity:")
                          (let [text-field (fx/text-field {:padding (Insets. 3 3 3 3) :pref-width 50} (str (.getValue arity-property)))]
                            (.addListener (.focusedProperty text-field)
                                          (reify ChangeListener
                                            (changed [this observable old new]
                                              (when-not new
                                                (.setValue arity-property (Long/valueOf ^String (.getText text-field)))))))
                            text-field))

            (VBox/setVgrow Priority/NEVER))
          (doto (fx/h-box {:padding   (Insets. 5 0 5 0)
                           :alignment (Pos/CENTER_LEFT)})
            (.. getChildren
                (setAll
                  (doto (FXCollections/observableArrayList
                          (for [i (range (:arity view-model))]
                            (fx/text-field {:padding     (Insets. 1 1 1 1)
                                            :min-height  Region/USE_COMPUTED_SIZE
                                            :prompt-text (str "term_" i)} "")))
                    (.add (fx/button {:min-width 40} "Add"))))

                )
            (VBox/setVgrow Priority/NEVER))
          (doto (fx/table-view {:editable true})
            (VBox/setVgrow Priority/ALWAYS)
            (.. getSelectionModel
                (setSelectionMode SelectionMode/MULTIPLE))
            (.. getSelectionModel
                (setCellSelectionEnabled true))
            (build-edit-table view-model)
            (.. getItems
                (setAll (->> (:data view-model)
                             (map (fn [row] (FXCollections/observableArrayList (->> row
                                                                                    (map (fn [el]
                                                                                           (SimpleStringProperty. el))))))))))))) )