(ns urdmi.gui.main
  (:require [clojure.core.async :refer [chan go <! >! put!]]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [urdmi.gui :as gui]
            [clojure.zip :as zip]
            [clojure.java.io :as io])
  (:import
    (javafx.scene.layout VBox Priority StackPane)
    (javafx.geometry Pos Insets Orientation)
    (javafx.scene.text Font)
    (javafx.scene.paint Color)
    (javafx.util Callback StringConverter)
    (javafx.scene.control.cell TextFieldTreeCell)
    (javafx.scene.control TreeView TreeItem ScrollPane MenuItem Tab Button TextArea ContextMenu)
    (javafx.scene.input KeyCode MouseEvent KeyCodeCombination)
    (org.controlsfx.control StatusBar)
    (javafx.beans.value ChangeListener)
    (java.awt Desktop)
    (javafx.event EventHandler)
    (java.text SimpleDateFormat)
    (java.util Date)))

(defn create-file-entry-context-menu [>app-requests tree-item-value]
  (let [base-menu (ContextMenu.)
        menu-type (first (:path tree-item-value))
        deletable (< 1 (count (:path tree-item-value)))
        make-file-entry-menu! (fn [text type]
                                (.. base-menu
                                    getItems
                                    (add
                                      (fx/menu-item {:text      text
                                                     :on-action (fn [e]
                                                                  (put! >app-requests {:type type
                                                                                       :key  (:path tree-item-value)}))}))))]

    (when (Desktop/isDesktopSupported)
      (make-file-entry-menu! "Open Location" :open-location))
    (when (= menu-type :relations)
      (make-file-entry-menu! "New Relation" :new-relation)
      (make-file-entry-menu! "Import Relation" :import-relation))

    (when (and deletable (or (= menu-type :output) (= menu-type :working-dir) (= menu-type :relations)))
      (make-file-entry-menu! "Delete" :delete-file))
    base-menu)
  )

(defn- build-file-menu-widget [>app-requests]
  (let [tree-view ^TreeView (fx/tree-view
                              {:show-root         false
                               :focus-traversable true
                               :cell-factory
                                                  (reify
                                                    Callback
                                                    (call [this tree-view]
                                                      (let [cell (TextFieldTreeCell. (proxy
                                                                                       [StringConverter] []
                                                                                       (toString [obj]

                                                                                         (str (:name obj)
                                                                                              (when (:modified obj)
                                                                                                "*"))
                                                                                         )))

                                                            cell-change (fn [new-data]
                                                                          (.remove (.getStyleClass cell) "error")
                                                                          (.setContextMenu cell (create-file-entry-context-menu >app-requests new-data))
                                                                          (if (:invalid new-data)
                                                                            (.add (.getStyleClass cell) "error")))
                                                            item-value-change-listener ^ChangeListener (reify ChangeListener
                                                                                                         (changed [this obs old new]
                                                                                                           (cell-change new)
                                                                                                           ))

                                                            item-change-listener (reify ChangeListener
                                                                                   (changed [this obs old new]
                                                                                     (when old
                                                                                       (.remove (.getStyleClass cell) "error")
                                                                                       (.removeListener (.valueProperty old) item-value-change-listener))
                                                                                     (when new
                                                                                       (.addListener (.valueProperty new) item-value-change-listener)
                                                                                       (if (.getValue new)
                                                                                         (cell-change (.getValue new))
                                                                                         ))))

                                                            ]
                                                        (.addListener (.treeItemProperty cell) item-change-listener)
                                                        cell)))})]
    (-> tree-view
        (.getSelectionModel)
        (.selectedItemProperty)
        (gui/on-changed
          (fn [obs old ^TreeItem new]
            (when new
              (.requestFocus tree-view)
              (put! >app-requests {:type :switch-page :target (:path (.getValue new))})))))

    (VBox/setVgrow tree-view Priority/ALWAYS)
    {:view tree-view :items (atom {})}))

(defn build-log-tab [tab-name]
  (let [text-area (fx/text-area {:editable false})
        tab (doto (Tab. tab-name text-area)
              (.setClosable false))
        ]
    [tab text-area])
  )

(defn build-logs-tabs []
  (let [[application-tab ^TextArea app-log-text-area] (build-log-tab "Application")
        [datamining-tab ^TextArea dm-log-text-area] (build-log-tab "Datamining")
        pane (fx/tab-pane)
        selected-tab (.selectedIndexProperty (.getSelectionModel pane))
        widget (fx/stack-pane)
        date-format (new SimpleDateFormat "yyyy-MM-dd HH:mm:ss")
        add-log-entry (fn [page-index text-area text]
                        (.appendText text-area (.format date-format (Date.)))
                        (.appendText text-area ": ")
                        (.appendText text-area text)
                        (.appendText text-area "\n")
                        (when (= 1 page-index)
                          (.select (.getSelectionModel pane) page-index)))]
    (gui/on-changed (.textProperty app-log-text-area) (fn [obs old new]
                                                        (.setScrollTop app-log-text-area Double/MAX_VALUE)))
    (gui/on-changed (.textProperty dm-log-text-area) (fn [obs old new]
                                                       (.setScrollTop dm-log-text-area Double/MAX_VALUE)))
    (doto widget
      (.. getChildren (add (doto pane
                             (.. getTabs (add application-tab))
                             (.. getTabs (add datamining-tab)))))
      (.. getChildren (add (doto ^Button (fx/button
                                           {:on-action (fn [e]
                                                         (case (.getValue selected-tab)
                                                           0 (.clear app-log-text-area)
                                                           1 (.clear dm-log-text-area)
                                                           ))}
                                           "Clear")
                             (StackPane/setAlignment Pos/TOP_RIGHT)
                             (StackPane/setMargin (Insets. 4 8 0 0)))))
      )
    [widget
     (fn [text]
       (add-log-entry 0 app-log-text-area text))
     (fn [text]
       (add-log-entry 1 dm-log-text-area text))]))

(defn- build-main-screen [>app-requests]
  (let [put-ui-event-fn (fn [event-data]
                          (fn [e]
                            (put! >app-requests event-data)))

        file-menu (build-file-menu-widget >app-requests)

        content-container (fx/scroll-pane :#content {:fit-to-height true
                                                     :fit-to-width  true})

        menu-items {:new-project  (fx/menu-item {:text "New" :on-action (put-ui-event-fn {:type :new-project}) :accelerator (gui/ctrl-key-accelerator KeyCode/N)})
                    :open-project (fx/menu-item {:text "Open..." :on-action (put-ui-event-fn {:type :open-project}) :accelerator (gui/ctrl-key-accelerator KeyCode/O)})
                    :build        (fx/menu-item {:text "Build" :disable true :on-action (put-ui-event-fn {:type :build}) :accelerator (gui/ctrl-key-accelerator KeyCode/B)})
                    :run          (fx/menu-item {:text "Run" :disable true :on-action (put-ui-event-fn {:type :run}) :accelerator (gui/ctrl-key-accelerator KeyCode/R)})
                    :build-run    (fx/menu-item {:text "Build and run" :disable true :on-action (put-ui-event-fn {:type :build-run}) :accelerator (KeyCodeCombination. KeyCode/R (into-array (list KeyCodeCombination/SHIFT_DOWN KeyCodeCombination/SHORTCUT_DOWN)))})
                    :save-file    (fx/menu-item {:text "Save" :disable true :on-action (put-ui-event-fn {:type :save-file}) :accelerator (gui/ctrl-key-accelerator KeyCode/S)})
                    :revert-file  (fx/menu-item {:text "Revert" :disable true :on-action (put-ui-event-fn {:type :revert-file})})
                    :reload-file  (fx/menu-item {:text "Reload" :disable true :on-action (put-ui-event-fn {:type :reload-file})})
                    :new-window   (fx/menu-item {:text "New window" :on-action (put-ui-event-fn {:type :new-window})})
                    :save-project (fx/menu-item {:text "Save all files" :disable true :on-action (put-ui-event-fn {:type :save-project})})
                    }

        [logs-tabs add-app-log-entry add-dm-log-entry] (build-logs-tabs)

        status-bar (doto (StatusBar.)
                     (.setText "Job: Idle")
                     (VBox/setVgrow Priority/NEVER)
                     )

        main-screen (fx/v-box {:pref-height       600
                               :pref-width        900
                               :focus-traversable true}
                              (doto (fx/menu-bar {}
                                                 (fx/menu {:text "Application"}
                                                          (:new-window menu-items))
                                                 (fx/menu {:text "Project"}
                                                          (:new-project menu-items)
                                                          (:open-project menu-items)
                                                          (:build menu-items)
                                                          (:run menu-items)
                                                          (:build-run menu-items)
                                                          (:save-project menu-items)
                                                          )
                                                 (fx/menu {:text "File"}
                                                          (:save-file menu-items)
                                                          (:revert-file menu-items)
                                                          (:reload-file menu-items)))
                                (.addEventFilter MouseEvent/MOUSE_CLICKED (reify EventHandler
                                                                            (handle [this e]
                                                                              (.requestFocus status-bar))))
                                (VBox/setVgrow Priority/NEVER))
                              (doto (fx/split-pane {:divider-positions (double-array [0.8])
                                                    :focus-traversable true
                                                    :orientation       Orientation/VERTICAL}
                                                   (doto (fx/split-pane {:divider-positions (double-array [0.25])
                                                                         :focus-traversable true}
                                                                        (fx/v-box {:focus-traversable true}
                                                                                  (:view file-menu))
                                                                        content-container))
                                                   logs-tabs)
                                (VBox/setVgrow Priority/ALWAYS))
                              status-bar
                              )]
    (.requestFocus main-screen)
    (gui/default-stylesheet main-screen)
    [main-screen file-menu content-container menu-items add-app-log-entry add-dm-log-entry status-bar]))

(deftype MainScreen [widget file-menu content-container menu-items app-requests add-app-log-entry add-dm-log-entry status-bar])

(defn make-main-screen [>app-requests]
  (let [[main-screen file-menu content-container menu-items add-app-log-entry add-dm-log-entry status-bar] (build-main-screen >app-requests)]
    (->MainScreen main-screen file-menu content-container menu-items >app-requests add-app-log-entry add-dm-log-entry status-bar)))

(defn get-parent-menu-item-for-path [path]
  (condp = path
    [:settings] :input
    [:relations] :input
    [:prolog-ext] :input
    [:working-dir] :output
    [:output] nil
    :input []
    :output []
    [] []
    (let [ppath (vec (butlast path))]
      (condp = ppath
        [:output] :output
        ppath)
      )))

(defn add-menu-files! [^MainScreen screen files-model]
  (let [{:keys [view items]} (.file_menu screen)]
    (doseq [{:keys [path name] :as data} files-model]
      (let [item (doto (fx/tree-item {:value data})
                   (.setExpanded false))]
        (condp = path []
                      (let [input-menu (doto (fx/tree-item {:value {:path [] :name "Input"}})
                                         (.setExpanded true))
                            output-menu (doto (fx/tree-item {:value {:path [] :name "Output"}})
                                          (.setExpanded true))]
                        (doto (.getChildren item)
                          (.add input-menu)
                          (.add output-menu))
                        (swap! items assoc :input input-menu)
                        (swap! items assoc :output output-menu)
                        (.setExpanded item true)
                        (.setRoot view item))
                      (when-let [parent-path (get-parent-menu-item-for-path path)]
                        (.add (.getChildren (get @items parent-path)) item)))
        (swap! items assoc path item)))))

(defn update-file-viewmodel! [^MainScreen screen path update-fn]
  (when-let [^TreeItem tree-item (get @(:items (.file_menu screen)) path)]
    (let [new-model (update-fn (.getValue tree-item))
          new-path (:path new-model)]
      (.setValue tree-item new-model)
      (when-not (= path new-path)
        (swap! (:items (.file_menu screen)) dissoc path)
        (swap! (:items (.file_menu screen)) assoc new-path tree-item)))))

(defn remove-file! [^MainScreen screen path]
  (if-let [^TreeItem item (get @(:items (.file_menu screen)) path)]
    (do
      (swap! (:items (.file_menu screen)) dissoc path)
      (.remove (.getChildren (.getParent item)) item))
    ))

(defn set-menu-files! [^MainScreen screen files-model]
  (.setRoot (:view (.file_menu screen)) nil)
  (swap! (:items (.file_menu screen)) {})
  (add-menu-files! screen files-model))

(defn set-menu-item-enabled! [^MainScreen screen menu-key enabled]
  (.setDisable ^MenuItem (menu-key (.menu-items screen)) (not enabled)))

(defn set-content-widget! [^MainScreen screen widget]
  (.setContent ^ScrollPane (.content_container screen)
               widget))

(defn add-app-log-entry! [^MainScreen screen text]
  ((.add-app-log-entry screen) text))

(defn add-dm-log-entry! [^MainScreen screen text]
  ((.add-dm-log-entry screen) text))

(defn start-job! [^MainScreen screen name ui-requests<]
  (doto
    (.status-bar screen)
    (.setText (str "Job: " name))
    (.setProgress -1)
    (.. getRightItems (add (fx/button {:text      "Stop"
                                       :on-action (fn [e]
                                                    (put! ui-requests< {:type :stop-job}))}))))
  )

(defn stop-job! [^MainScreen screen]
  (doto
    (.status-bar screen)
    (.setText (str "Job: Idle"))
    (.. getRightItems clear)
    (.setProgress 0.0)))

(defn get-widget [^MainScreen screen]
  (.widget screen))

;(fx/sandbox #'build-logs-tabs)

