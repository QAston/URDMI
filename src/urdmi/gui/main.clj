(ns urdmi.gui.main
  (:require [clojure.core.async :refer [chan go <! >! put!]]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [urdmi.gui :as gui]
            [clojure.zip :as zip]
            [clojure.java.io :as io])
  (:import
    (javafx.scene.layout AnchorPane Region VBox Priority HBox)
    (javafx.geometry Pos Insets)
    (javafx.scene.text Font TextAlignment)
    (javafx.scene.paint Color)
    (javafx.util Callback StringConverter)
    (javafx.scene.control.cell TextFieldTreeCell)
    (javafx.scene.control TreeView TreeItem ScrollPane MenuItem)
    (javafx.stage FileChooser DirectoryChooser)
    (java.io File)
    (javafx.scene.input KeyCodeCombination KeyCode)))

(defn- build-file-menu-widget [>app-requests]
  (let [tree-view ^TreeView (fx/tree-view
                              {:focus-traversable true
                               :cell-factory
                                                  (reify
                                                    Callback
                                                    (call [this tree-view]
                                                      (TextFieldTreeCell. (proxy
                                                                            [StringConverter] []
                                                                            (toString [obj]

                                                                              (str (:name obj)
                                                                                   (when (:modified obj)
                                                                                     "*"))
                                                                              )))))})]
    (-> tree-view
        (.getSelectionModel)
        (.selectedItemProperty)
        (gui/on-changed
          (fn [obs old ^TreeItem new]
            (put! >app-requests {:type :switch-page :target (:path (.getValue new))}))))
    (VBox/setVgrow tree-view Priority/ALWAYS)
    {:view tree-view :items (atom {})}))

(defn- build-main-screen [>app-requests]
  (let [font (Font/font 11.0)
        text-fill (Color/color 0.625 0.625 0.625)

        put-ui-event-fn (fn [event-data]
                          (fn [e]
                            (put! >app-requests event-data)))

        file-menu (build-file-menu-widget >app-requests)

        content-container (fx/scroll-pane :#content {:fit-to-height true
                                                     :fit-to-width  true})

        menu-items {:new-project  (fx/menu-item {:text "New" :disable true :on-action (put-ui-event-fn {:type :new-project}) :accelerator (gui/ctrl-key-accelerator KeyCode/N)})
                    :open-project (fx/menu-item {:text "Open..." :on-action (put-ui-event-fn {:type :open-project}) :accelerator (gui/ctrl-key-accelerator KeyCode/O)})
                    :build        (fx/menu-item {:text "Build" :disable true :on-action (put-ui-event-fn {:type :build}) :accelerator (gui/ctrl-key-accelerator KeyCode/B)})
                    :run          (fx/menu-item {:text "Run" :disable true :on-action (put-ui-event-fn {:type :run}) :accelerator (gui/ctrl-key-accelerator KeyCode/R)})
                    :build-run    (fx/menu-item {:text "Build and run" :disable true :on-action (put-ui-event-fn {:type :build-run})})
                    :save-file    (fx/menu-item {:text "Save" :disable true :on-action  (put-ui-event-fn {:type :save-file}) :accelerator (gui/ctrl-key-accelerator KeyCode/S)})
                    :revert-file  (fx/menu-item {:text "Revert" :disable true :on-action (put-ui-event-fn {:type :revert-file})})
                    }

        main-screen (fx/v-box {:pref-height 600
                               :pref-width  900}
                              (doto (fx/menu-bar
                                      (fx/menu {:text "Project"}
                                               (:new-project menu-items)
                                               (:open-project menu-items)
                                               (:build menu-items)
                                               (:run menu-items)
                                               (:build-run menu-items)
                                               )
                                      (fx/menu {:text "File"}
                                               (:save-file menu-items)
                                               (:revert-file menu-items)))
                                (VBox/setVgrow Priority/NEVER))
                              (doto (fx/split-pane {:divider-positions (double-array [0.25])
                                                    :focus-traversable true}
                                                   (fx/v-box {:focus-traversable true}
                                                             (:view file-menu))
                                                   content-container)

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
    (.. main-screen getStylesheets (add (.toExternalForm (io/resource "main.css"))))
    [main-screen file-menu content-container menu-items]))

(deftype MainScreen [widget file-menu content-container menu-items app-requests])

(defn make-main-screen [>app-requests]
  (let [[main-screen file-menu content-container menu-items] (build-main-screen >app-requests)]
    (->MainScreen main-screen file-menu content-container menu-items >app-requests)))

(defn add-menu-files! [^MainScreen screen files-model]
  (let [{:keys [view items]} (.file_menu screen)]
    (doseq [{:keys [path name] :as data} files-model]
      (let [item (doto (fx/tree-item {:value data})
                   (.setExpanded true))]
        (if (= path [])
          (.setRoot view item)
          (.add (.getChildren (get @items (vec (butlast path)))) item))
        (swap! items assoc path item)))))

(defn update-file-viewmodel! [^MainScreen screen path update-fn]
  (let [^TreeItem tree-item (get @(:items (.file_menu screen)) path)
        new-model (update-fn (.getValue tree-item))
        new-path (:path new-model)]
    (.setValue tree-item new-model)
    (when-not (= path new-path)
      (swap! (:items (.file_menu screen)) dissoc path)
      (swap! (:items (.file_menu screen)) assoc new-path tree-item))))

(defn remove-file! [^MainScreen screen path]
  (let [^TreeItem item (get @(:items (.file_menu screen)) path)]
    (swap! (:items (.file_menu screen)) dissoc path)
    (.remove (.getChildren (.getParent item)) item)
    ))

(defn set-menu-files! [^MainScreen screen files-model]
  (.setRoot (:view (.file_menu screen)) nil)
  (swap! (:items (.file_menu screen)) {})
  (add-menu-files! screen files-model))

(defn set-menu-item-enabled![^MainScreen screen menu-key enabled]
  (.setDisable ^MenuItem (menu-key (.menu-items screen)) (not enabled)))

(defn set-content-widget! [^MainScreen screen widget]
  (.setContent ^ScrollPane (.content_container screen)
               widget))

(defn get-widget [^MainScreen screen]
  (.widget screen))

(defn open-project-dialog [stage ^File dir]
  (.showDialog
    (doto (DirectoryChooser.)
      (.setInitialDirectory dir)
      (.setTitle "Select project dir")
      )
    stage))

