(ns urdmi.gui.relation
  (:require [clojure.core.async :refer [chan go <! >!]]
            [fx-clj.core :as fx]
            [urdmi.prolog :as prolog]
            [clojure.string :as string]
            [urdmi.util :as util]
            [urdmi.gui :as gui])
  (:import
    (javafx.scene.layout Region VBox Priority HBox)
    (javafx.geometry Pos Insets)
    (javafx.scene.control TableColumn TableView SelectionMode ContextMenu MenuItem TablePosition TextField TableColumn$CellEditEvent)
    (javafx.collections ObservableList ListChangeListener)
    (java.util List Collection)
    (javafx.util Callback)
    (javafx.scene.control.cell TextFieldTableCell)
    (java.io StringWriter)
    (javafx.beans.value ChangeListener)
    (javafx.beans.property StringProperty SimpleStringProperty SimpleLongProperty)
    (javafx.scene.input KeyCodeCombination KeyCode Clipboard ClipboardContent)
    (javafx.event EventHandler)
    (javafx.util.converter DefaultStringConverter)
    (org.apache.commons.lang3.reflect FieldUtils)
    (org.controlsfx.validation.decoration GraphicValidationDecoration)
    (org.controlsfx.validation ValidationSupport)))

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

(def default-cell-value "_")

(defn- new-relation-row [arity]
  (gui/observable-list (for [i (range arity)]
                         (SimpleStringProperty. default-cell-value))))

(defn- set-data-cols [data new-cols-cnt]
  (when-not (.isEmpty data)
    (dotimes [row-index (.size data)]
      (let [row (.get data row-index)]
        (gui/resize-observable-list row new-cols-cnt (fn [i]
                                                       (SimpleStringProperty. default-cell-value)))
        ))
    ))

(defn- set-data-rows [data new-row-cnt]
  (when-not (.isEmpty data)
    (let [arity (count (first data))]
      (gui/resize-observable-list data new-row-cnt (fn [i]
                                                     (new-relation-row arity)))
      )))

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

(defn- paste-action [in-string ^TableView relation-table arity-property]
  (when (and in-string (not (.. relation-table getSelectionModel getSelectedItems isEmpty)))
    (let [data (for [line (string/split-lines in-string)]
                 (string/split line #"\t"))
          [left-bound top-bound right-bound bottom-bound] (calc-cell-bounds
                                                            (seq (.. relation-table
                                                                     getSelectionModel
                                                                     getSelectedCells)))
          items (.getItems relation-table)
          pasted-rows (count data)
          pasted-cols (apply max (map count data))
          new-row-cnt (+ pasted-rows top-bound)
          new-col-cnt (+ pasted-cols left-bound)
          row-cnt (count items)
          col-cnt (count (first items))
          ]
      (when (< row-cnt new-row-cnt)
        (set-data-rows items new-row-cnt))
      (when (< col-cnt new-col-cnt)
        (.setValue arity-property new-col-cnt))
      (doseq [[row-index src-row] (map-indexed util/args-vec data)
              :let [^ObservableList target-row (.get items (+ top-bound row-index))]]
        (doseq [[col-index src-col] (map-indexed util/args-vec src-row)]
          (.setValue ^StringProperty (.get target-row (+ col-index left-bound)) src-col)
          )
        ))))

(defn- get-selected-row-indexes-in-reverse-order [^TableView relation-table]
  (let [items (seq (.. relation-table
                       getSelectionModel
                       getSelectedCells))]
    (->> items
         (map (fn [^TablePosition tp]
                (.getRow tp)))
         (sort)
         (reverse)
         (distinct)
         )
    ))

(defn- delete-rows-action [^TableView relation-table]
  (doseq [row-index (get-selected-row-indexes-in-reverse-order relation-table)]
    (.remove (.getItems relation-table) (int row-index)))
  )

(defn- insert-row-action [^TableView relation-table]
  (doseq [row-index (get-selected-row-indexes-in-reverse-order relation-table)
          arity (count (first (.getItems relation-table)))]
    (.add (.getItems relation-table)
          row-index
          (new-relation-row arity)
          )))

(defn- build-context-menu [^TableView relation-table arity-property]
  (doto (fx/context-menu)
    (.. getItems
        (add (fx/menu-item {:text        "Copy"
                            :accelerator (KeyCodeCombination. KeyCode/C (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                            :on-action   (fn [e]
                                           (.setContent (Clipboard/getSystemClipboard)
                                                        (doto (ClipboardContent.)
                                                          (.putString (copy-action relation-table)))))})))
    (.. getItems
        (add (fx/menu-item {:text        "Paste"
                            :accelerator (KeyCodeCombination. KeyCode/V (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                            :on-action   (fn [e]
                                           (paste-action
                                             (.getString (Clipboard/getSystemClipboard))
                                             relation-table
                                             arity-property))})))
    (.. getItems
        (add (fx/menu-item {:text      "Delete selected rows"
                            :on-action (fn [e]
                                         (delete-rows-action relation-table))})))
    (.. getItems
        (add (fx/menu-item {:text      "Insert row above"
                            :on-action (fn [e]
                                         (insert-row-action relation-table))})))
    ))

(defn- column-cell-factory [^TableView relation-table ^ContextMenu context-menu col-index]
  (reify Callback
    (call [this table-column]
      (doto (proxy [TextFieldTableCell] [(DefaultStringConverter.)]
              (startEdit []
                (let [cell this
                      oldTextField (FieldUtils/readField this "textField", true)
                      _ (proxy-super startEdit)
                      ^TextField newTextField (FieldUtils/readField this "textField", true)]
                  (when-not (identical? oldTextField newTextField)
                    (gui/on-changed (.focusedProperty newTextField)
                                    (fn [observale old new]
                                      (when-not new

                                        (.commitEdit ^TextFieldTableCell cell
                                                     (.getText newTextField))

                                        ;this is a hack, because isEditing is false
                                        ;when changing to another cell
                                        (let [row-index (.getIndex cell)
                                              cell-val (.get (.get (.getItems relation-table) row-index) col-index)
                                              selected-cell-pos (first (.. relation-table
                                                                           getSelectionModel
                                                                           getSelectedCells))]
                                          (when-not (and selected-cell-pos (or (not= (.getColumn selected-cell-pos) col-index) (not= (.getRow selected-cell-pos) row-index))
                                                         (.setValue cell-val (.getText newTextField))))))))
                    ))))
        (.setContextMenu context-menu)))
    ))

(defn- build-table-columns [^TableView relation-table arity-property column-widths]
  (let [context-menu (build-context-menu relation-table arity-property)
        column-factory (fn [col-index]
                         (let [col-width (.get column-widths col-index)
                               col (doto (TableColumn. (str "term_" col-index))
                                     (.setEditable true)
                                     (.setCellFactory (column-cell-factory ^TableView relation-table context-menu col-index))
                                     (.setOnEditCommit (reify EventHandler
                                                         (handle [this cell-edit-event]
                                                           (let [row (.getRowValue cell-edit-event)
                                                                 col-index (.. cell-edit-event getTablePosition getColumn)
                                                                 newValue (.getNewValue cell-edit-event)]
                                                             (.setValue (.get row col-index) newValue)))))
                                     (.setCellValueFactory (reify Callback
                                                             (call [this cell-data-features]
                                                               (.get (.getValue cell-data-features) col-index))
                                                             )))]
                           (.bind col-width (.widthProperty col))
                           col))]
    (gui/on-changed arity-property
                    (fn [obs old-size new-size]
                      (gui/resize-observable-list (.. relation-table
                                                      getColumns) new-size column-factory)))
    ))

(defn- build-name-arity-widget [^SimpleStringProperty name-property ^SimpleLongProperty arity-property ^ValidationSupport validation]
  (let [widget (doto (fx/h-box {:padding   (Insets. 5 5 5 5)
                                :alignment (Pos/CENTER_LEFT)}
                               (fx/label {:padding (Insets. 3 3 3 3)} "Name:")
                               (let [text-field (fx/text-field {:padding (Insets. 3 3 3 3)} (.getValue name-property))]
                                 (gui/on-changed (.focusedProperty text-field)
                                                 (fn [observable old new]
                                                   (when-not new
                                                     (.setValue name-property (.getText text-field)))))
                                 (gui/on-changed name-property
                                                 (fn [obs old new]
                                                   (when (not= old new)
                                                     (.setText text-field new))))
                                 text-field)
                               (fx/label {:padding (Insets. 3 3 3 13)} "Arity:")
                               (let [text-field (fx/text-field {:padding (Insets. 3 3 3 3) :pref-width 50} (str (.getValue arity-property)))
                                     test-fn (fn [s]
                                               (try
                                                 (>= (Long/parseLong ^String s) 0)
                                                 (catch NumberFormatException e
                                                   false)))
                                     update-arity-prop (fn[]
                                                         (let [text (.getText text-field)]
                                                           (when (test-fn text)
                                                             (.setValue arity-property (Long/valueOf ^String text)))))
                                     ]

                                 (gui/validate-control validation text-field test-fn
                                                       "Arity must be a number > 0")
                                 (gui/on-changed (.focusedProperty text-field)
                                                 (fn [observable old new]
                                                   (when-not new
                                                     (update-arity-prop))))

                                 (.setOnAction text-field (reify EventHandler
                                                            (handle [this e]
                                                              (update-arity-prop))))

                                 (gui/on-changed arity-property
                                                 (fn [obs old new]
                                                   (when (not= old new)
                                                     (.setText text-field (str new)))))


                                 text-field))

                 (VBox/setVgrow Priority/NEVER))]

    widget))

(defn- build-new-row-widget [^SimpleLongProperty arity-property column-widths data]
  (let [widget (doto (fx/h-box {:padding   (Insets. 5 1 5 1)
                                :alignment (Pos/CENTER_LEFT)
                                :spacing   1})

                 (VBox/setVgrow Priority/NEVER))
        new-row-data (gui/observable-list)
        add-button (fx/button {:min-width 40
                               :on-action (fn [e]
                                            (.add data
                                                  (gui/observable-list
                                                    (for [prop new-row-data]
                                                      (SimpleStringProperty. (.getValue prop)))))
                                            (doseq [prop new-row-data]
                                              (.setValue prop ""))
                                            (.. widget
                                                getChildren
                                                (get 0)
                                                (requestFocus))
                                            )} "Add")]
    (gui/on-changed arity-property
                    (fn [observer old-size new-size]
                      (let [diff (- new-size old-size)]
                        (if (>= diff 0)
                          (do
                            (.addAll
                              new-row-data
                              (for [i (range diff)]
                                (SimpleStringProperty. "")))
                            (doto (.. widget
                                      getChildren)
                              (.remove add-button)
                              (.addAll (for [i (range diff)
                                             :let [col-width (.get column-widths (+ old-size i))
                                                   text-field (doto (fx/text-field {:padding     (Insets. 1)
                                                                                    :min-height  Region/USE_COMPUTED_SIZE
                                                                                    :prompt-text (str "term_" (+ old-size i))} ""))]]

                                         (do
                                           (.bindBidirectional (.get new-row-data i) (.textProperty text-field))
                                           (.bind (.maxWidthProperty text-field) col-width)
                                           (.bind (.minWidthProperty text-field) col-width)
                                           text-field)))
                              (.add add-button)))
                          (do
                            (.. widget
                                getChildren
                                (remove new-size old-size))
                            (.remove new-row-data new-size old-size)))
                        )))
    widget))

(defn- build-table-widget [table-data arity-property column-widths]
  (doto (fx/table-view {:editable true})
    (VBox/setVgrow Priority/ALWAYS)
    (.. getSelectionModel
        (setSelectionMode SelectionMode/MULTIPLE))
    (.. getSelectionModel
        (setCellSelectionEnabled true))
    (build-table-columns arity-property column-widths)
    (.setItems table-data)))

;usability improvements:
; term warning in relation
; term warning in addition
; edit table when typing
; select next on enter when editing table
;todo: on file save expressions that are not valid prolog should be put in urdmi_editor("expr") to be safely saved
;a panel with a table view with remove add column buttons and sorting
;also rename relation textbox
;todo: have mutuable model, which is mapped to a persistent data structure for history support on change commit
;multiple selection: ctrl + click on cell to add cell to the selection
; shift + click, add all cells in bettween
(defn build-relation-edit-widget [view-model]
  (let [name-property (SimpleStringProperty. "")
        arity-property (SimpleLongProperty. 0)
        column-widths (gui/observable-list)
        _ (gui/on-changed arity-property
                          (fn [obs old-size new-size]
                            (gui/resize-observable-list column-widths new-size (fn [i]
                                                                                 (SimpleLongProperty. 0)))))
        data (gui/observable-list)
        validation (gui/validation-support (GraphicValidationDecoration.))
        widget (list (build-name-arity-widget name-property arity-property validation)
                     (build-new-row-widget arity-property column-widths data)
                     (build-table-widget data arity-property column-widths))]

    (gui/on-changed arity-property
                    (fn [obs old-size new-size]
                      (set-data-cols data new-size)))
    (.setAll data ^Collection (->> (:data view-model)
                                   (map (fn [row] (gui/observable-list (->> row
                                                                            (map (fn [el]
                                                                                   (SimpleStringProperty. el)))))))))
    (.setValue arity-property (:arity view-model))
    (.setValue name-property (:name view-model))

    (doto (fx/v-box {:focus-traversable true
                     :max-height        Double/MAX_VALUE
                     :max-width         Double/MAX_VALUE})
      (.. getChildren (setAll (gui/observable-list widget))))
    ))

(defn test-fn []
  (build-relation-edit-widget {:name  "dzial"
                               :arity 6
                               :data  [["1" "produkcja" "produkcyjna" "1" "null" "lapy"]
                                       ["2" "sprzedaz" "lipowa" "1" "1" "bialystok"]
                                       ["3" "kontrolajakosci" "produkcyjna" "1" "1" "lapy"]
                                       ["4" "marketing" "lipowa" "1" "2" "bialystok"]
                                       ["5" "ksiegowosc" "lipowa" "1" "3" "bialystok"]
                                       ["6" "informatyka" "lipowa" "1" "4" "bialystok"]
                                       ["7" "reklamacja" "lipowa" "1" "5" "bialystok"]
                                       ["8" "informatyka" "produkcyjna" "1" "1" "lapy"]]}))
(fx/sandbox #'test-fn)