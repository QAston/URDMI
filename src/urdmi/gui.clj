(ns urdmi.gui
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [fx-clj.core :as fx]
            [differ.core :as differ]
            [urdmi.core :as core]
            [clojure.zip :as zip]
            [urdmi.prolog :as prolog]
            [clojure.string :as string]
            [urdmi.util :as util])
  (:import [javafx.beans.property.StringProperty]
           (javafx.scene.layout AnchorPane Region VBox Priority HBox)
           (javafx.geometry Pos Insets)
           (javafx.scene.text Font TextAlignment)
           (javafx.scene.paint Color)
           (javafx.scene.control TreeItem TableCell TableRow TreeCell TableColumn TableView SelectionMode ContextMenu MenuItem TablePosition TextField)
           (javafx.collections ObservableList FXCollections)
           (java.util ArrayList List)
           (urdmi.core Project)
           (javafx.util Callback StringConverter)
           (javafx.scene.control.cell TextFieldTreeCell TextFieldTableCell CellUtils)
           (com.ugos.jiprolog.engine OperatorManager)
           (java.io StringWriter)
           (javafx.beans.value ObservableStringValue ChangeListener)
           (javafx.beans.property StringProperty SimpleStringProperty)
           (javafx.scene.input KeyCodeCombination KeyCode Clipboard ClipboardContent)
           (javafx.event EventHandler)
           (javafx.util.converter DefaultStringConverter)
           (org.apache.commons.lang3.reflect FieldUtils)))

(defn load-fxml [filename]
  (let [loader (new javafx.fxml.FXMLLoader (io/resource filename))]
    (.load loader)))

(defn create-view []
  (let [click-ch (chan)
        btn (fx/button :#my-btn {:on-action click-ch        ;; You can bind a core.async channel directly to an event
                                 :text      "Next"})

        txt (fx/text "Initial text")
        view (fx/v-box txt btn)]

    (go
      (<! click-ch)
      (fx/run<! (fx/pset! txt "Next text"))
      (<! click-ch)
      (fx/run<!
        (fx/pset! txt "Last text")
        (fx/pset! btn {:text "Done"}))
      (println "Done listening to clicks"))

    view))

; property as clojure ref
(comment @(fx/property-ref node :text))
; lookup by id
(comment (fx/lookup node :#id))

(defn load-main-view []
  (load-fxml "main.fxml"))

(defn- file-names-recursively [zipiter proj-key]
  (let [path (into [proj-key] (conj (mapv :name (rest (zip/path zipiter))) (:name (zip/node zipiter))))
        name (:name (zip/node zipiter))]
    (if (zip/branch? zipiter)
      (vector
        (keep identity (cons [:name (:name (zip/node zipiter)) :path path]
                             (if-let [childiter (zip/down zipiter)]
                               (loop [iter childiter
                                      children []
                                      ]
                                 (if-not (nil? iter)
                                   (recur (zip/right iter) (conj children (file-names-recursively iter proj-key)))
                                   children))

                               (list)
                               ))))
      {:name name :path path})))

(defn- get-file-names [^Project p proj-key display-name]
  (vec (cons {:name display-name :path [proj-key]}
             (rest (first (file-names-recursively (core/file-model-zipper (get-in p (core/dir-keys proj-key))) proj-key))))))

(defn generate-menu-viewmodel [^Project p]
  (let [
        relations (get-file-names p core/relations-keyname "Relations")
        working-dir (get-file-names p core/workdir-keyname "Working dir")
        outputs (get-file-names p core/output-keyname "Output")
        additions (get-file-names p core/output-keyname "Additions")
        settings (get-file-names p core/output-keyname "Settings")
        ]
    [{:name "Project" :path []} relations working-dir outputs additions settings]))

(defn set-widget-children [node children]
  (fx/run!
    (.. node
        (getChildren)
        (setAll
          children))))

(defn- build-file-menu-entry-widget [data]
  (if-not (vector? data)
    (fx/tree-item {:value data})
    (let [dir-data (first data)]
      (doto (fx/tree-item {:value dir-data})
        (.. getChildren
            (setAll
              (for [entry (rest data)]
                (build-file-menu-entry-widget entry))))
        ))))

(defn update-main-file-menu
  [view]
  (set-widget-children (fx/lookup view :#file-selection)
                       (list (doto (fx/tree-view
                                     {:cell-factory (reify
                                                      Callback
                                                      (call [this tree-view]
                                                        (TextFieldTreeCell. (proxy
                                                                              [StringConverter] []
                                                                              (toString [obj]
                                                                                (:name obj)
                                                                                )))))
                                      :root         (doto (build-file-menu-entry-widget [{:name "Project", :path []}
                                                                                         [{:name "Relations", :path [:relations]}
                                                                                          {:name "towar_6.pl", :path [:relations "towar_6.pl"]}
                                                                                          {:name "produkcja_5.pl", :path [:relations "produkcja_5.pl"]}
                                                                                          {:name "pracownik_7.pl", :path [:relations "pracownik_7.pl"]}
                                                                                          {:name "pracownikpersonalia_8.pl",
                                                                                           :path [:relations "pracownikpersonalia_8.pl"]}
                                                                                          {:name "klient_9.pl", :path [:relations "klient_9.pl"]}
                                                                                          {:name "zamowienieszczegoly_4.pl",
                                                                                           :path [:relations "zamowienieszczegoly_4.pl"]}
                                                                                          {:name "pracownikprodukcja_7.pl",
                                                                                           :path [:relations "pracownikprodukcja_7.pl"]}
                                                                                          {:name "zamowienie_5.pl", :path [:relations "zamowienie_5.pl"]}
                                                                                          {:name "dzial_6.pl", :path [:relations "dzial_6.pl"]}]
                                                                                         [{:name "Working dir", :path [:working-dir]}
                                                                                          {:name "pracownik.b", :path [:working-dir "pracownik.b"]}
                                                                                          {:name "pracownik.f", :path [:working-dir "pracownik.f"]}
                                                                                          {:name "pracownik.n", :path [:working-dir "pracownik.n"]}]
                                                                                         [{:name "Output", :path [:output]}
                                                                                          {:name "result.edn", :path [:output "result.edn"]}]
                                                                                         [{:name "Additions", :path [:output]}
                                                                                          {:name "result.edn", :path [:output "result.edn"]}]
                                                                                         [{:name "Settings", :path [:output]}
                                                                                          {:name "result.edn", :path [:output "result.edn"]}]])
                                                      (.setExpanded true))}
                                     )
                               (VBox/setVgrow Priority/ALWAYS))))
  )

(defn- generate-rel-ast [rel-asts]
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

(defn generate-relations-viewmodel [rel]
  (let [rel-asts (:ast rel)
        [rel-name rel-arity] (:rel rel)]
    {:name  rel-name
     :arity rel-arity
     :data  (generate-rel-ast rel-asts)}
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

(defn- relation-edit-copy [^TableView relation-table]
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
                             (.append builder (.get ^List (.get (.getItems relation-table)
                                                                (.getRow next)) (.getColumn next)))
                             (when-not (= (.getColumn next) right-bound)
                               (.append builder \tab))
                             (recur (rest cells) builder row (inc (.getColumn next))))
                           ))))]
      data)))

(defn- relation-edit-paste [in-string ^TableView relation-table]
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
            (.set target-row (+ col-index left-bound) src-col))
          )
        ;this refreshes the table
        (.set items (+ top-bound row-index) target-row))
      )))

(defn- relation-edit-context-menu [^TableView relation-table]
  (doto (ContextMenu.)
    (.. getItems
        (add (doto (MenuItem. "Copy")
               (.setAccelerator (KeyCodeCombination. KeyCode/C (into-array (list KeyCodeCombination/SHORTCUT_DOWN))))
               (.setOnAction (reify EventHandler
                               (handle [this action-event]
                                 (.setContent (Clipboard/getSystemClipboard)
                                              (doto (ClipboardContent.)
                                                (.putString (relation-edit-copy relation-table))))
                                 ))))))
    (.. getItems
        (add (doto (MenuItem. "Paste")
               (.setAccelerator (KeyCodeCombination. KeyCode/V (into-array (list KeyCodeCombination/SHORTCUT_DOWN))))
               (.setOnAction (reify EventHandler
                               (handle [this action-event]
                                 (relation-edit-paste
                                   (.getString (Clipboard/getSystemClipboard))
                                   relation-table)))))))
    ))

(gen-class
  :name "urdmi.gui.RelationsTableCell"
  :extends TextFieldTableCell
  :exposes-methods {startEdit superStartEdit}
  :prefix "relations-table-cell-")

(defn relations-table-cell-startEdit [this]
  (let [cell this
        oldTextField (FieldUtils/readField this "textField", true)
        _ (.superStartEdit this)
        ^TextField newTextField (FieldUtils/readField this "textField", true)]
    (when-not (identical? oldTextField newTextField)
      (.addListener (.focusedProperty newTextField)
                    (reify ChangeListener
                      (changed [this observale old new]
                        (when-not new
                          (.commitEdit cell (.getText newTextField)))))))))

(defn- relation-edit-table [^TableView relation-table view-model]
  (let [context-menu (relation-edit-context-menu relation-table)]
    (.. relation-table
        getColumns
        (setAll (for [i (range (:arity view-model))]
                  (doto (TableColumn. (str "col_" i))
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
                                                                           (.commitEdit cell (.getText newTextField))))))))))
                                           (.setContextMenu context-menu)))
                                       ))
                    (.setCellValueFactory (reify Callback
                                            (call [this cell-data-features]
                                              (SimpleStringProperty. (str (nth (.getValue cell-data-features) i))))
                                            ))))))
    ))

;a panel with a table view with remove add column buttons and sorting
;also rename relation textbox
;todo: have mutuable model, which is mapped to a persistent data structure for history support on change commit
;todo: confirmation on lost focus
;multiple selection: ctrl + click on cell to add cell to the selection
; shift + click, add all cells in bettween
(defn build-relation-edit-widget [view-model]
  (list (doto (fx/h-box
                (fx/label "Name:") (fx/text-field (:name view-model)))
          (VBox/setVgrow Priority/NEVER))
        (doto (fx/h-box
                (fx/label "Arity:") (fx/text-field (str (:arity view-model))))
          (VBox/setVgrow Priority/NEVER))
        (doto (fx/table-view {:editable true})
          (VBox/setVgrow Priority/ALWAYS)
          (.. getSelectionModel
              (setSelectionMode SelectionMode/MULTIPLE))
          (.. getSelectionModel
              (setCellSelectionEnabled true))
          (relation-edit-table view-model)
          (.. getItems
              (setAll (map (fn [a] (FXCollections/observableArrayList a)) (:data view-model)))))))

;additions edition:
;just a large text area
;there's undo/redo support
;http://fxexperience.com/controlsfx/ the best control lib so far

(defn new-main-view []
  (let [font (Font/font 11.0)
        text-fill (Color/color 0.625 0.625 0.625)
        menu-events (chan)
        file-events (chan)

        view (fx/v-box {:pref-height 600
                        :pref-width  900
                        }
                       (doto (fx/menu-bar
                               (fx/menu {:text "Project"}
                                        (fx/menu-item {:text "New" :on-action menu-events})
                                        (fx/menu-item {:text "Open..." :on-action menu-events})))
                         (VBox/setVgrow Priority/NEVER))
                       (doto (fx/split-pane {:divider-positions (double-array [0.25])
                                             :focus-traversable true
                                             :pref-height       Region/USE_COMPUTED_SIZE
                                             :pref-width        Region/USE_COMPUTED_SIZE}

                                            (doto
                                              (fx/v-box :#file-selection {:focus-traversable true})
                                              (VBox/setVgrow Priority/ALWAYS))

                                            (doto
                                              (fx/v-box :#content {:focus-traversable true})
                                              (VBox/setVgrow Priority/ALWAYS)))

                         (VBox/setVgrow Priority/ALWAYS))
                       (doto (fx/h-box :#hbox {:alignment Pos/CENTER_LEFT
                                               :spacing   5.0
                                               :padding   (Insets. 3 3 3 3)}
                                       (doto (fx/label {:max-height 1.8
                                                        :max-width  Region/USE_COMPUTED_SIZE
                                                        :text       "Left status"
                                                        :font       font
                                                        :text-fill  text-fill})
                                         (HBox/setHgrow Priority/ALWAYS))
                                       (doto (fx/pane {:pref-height Region/USE_COMPUTED_SIZE
                                                       :pref-width  Region/USE_COMPUTED_SIZE})
                                         (HBox/setHgrow Priority/ALWAYS))
                                       (doto (fx/label {:max-height 1.8
                                                        :max-width  Region/USE_COMPUTED_SIZE
                                                        :text       "Right status"
                                                        :font       font
                                                        :text-fill  text-fill})
                                         (HBox/setHgrow Priority/NEVER)))
                         (VBox/setVgrow Priority/NEVER))
                       )]
    ; update file menu in place
    ;(update-main-file-menu view)
    (go
      (while true

        (<! file-events)
        ))

    (go
      (let [^javafx.event.ActionEvent e (<! menu-events)]
        ))

    (def main-view view)
    view
    ))

(def create-main-view new-main-view)

(fx/sandbox #'create-main-view)
(update-main-file-menu main-view)
(set-widget-children (fx/lookup main-view :#content)
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
(comment
  (fx/sandbox #'create-main-view))



