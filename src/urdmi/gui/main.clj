(ns urdmi.gui.main
  (:require [clojure.core.async :refer [chan go <! >! put!]]
            [fx-clj.core :as fx]
            [urdmi.core :as core]
            [urdmi.gui :as gui]
            [clojure.zip :as zip])
  (:import
    (javafx.scene.layout AnchorPane Region VBox Priority HBox)
    (javafx.geometry Pos Insets)
    (javafx.scene.text Font TextAlignment)
    (javafx.scene.paint Color)
    (javafx.util Callback StringConverter)
    (javafx.scene.control.cell TextFieldTreeCell)
    (javafx.scene.control TreeView TreeItem)
    (javafx.stage FileChooser DirectoryChooser)))

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

(defn- build-file-menu-widget [files-view-model >app-requests]
  (let [tree-view ^TreeView (fx/tree-view
                              {:cell-factory (reify
                                               Callback
                                               (call [this tree-view]
                                                 (TextFieldTreeCell. (proxy
                                                                       [StringConverter] []
                                                                       (toString [obj]
                                                                         (:name obj)
                                                                         )))))
                               :root         (doto (build-file-menu-entry-widget files-view-model)
                                               (.setExpanded true))})]
    (-> tree-view
        (.getSelectionModel)
        (.selectedItemProperty)
        (gui/on-changed
          (fn [obs old ^TreeItem new]
            (put! >app-requests {:type :switch-page :target (:path (.getValue new))}))))
    (VBox/setVgrow tree-view Priority/ALWAYS)
    tree-view))

(defn- build-main-screen [>app-requests]
  (let [font (Font/font 11.0)
        text-fill (Color/color 0.625 0.625 0.625)

        put-ui-event-fn (fn [event-data]
                          (fn [e]
                            (put! >app-requests event-data)))

        file-menu-container (fx/v-box :#file-selection {:focus-traversable true})

        content-container (fx/scroll-pane :#content {:fit-to-height true
                                                     :fit-to-width  true})

        main-screen (fx/v-box {:pref-height 600
                               :pref-width  900}
                              (doto (fx/menu-bar
                                      (fx/menu {:text "Project"}
                                               (fx/menu-item {:text "New" :on-action (put-ui-event-fn {:type :new-project})})
                                               (fx/menu-item {:text "Open..." :on-action (put-ui-event-fn {:type :open-project})})
                                               (fx/menu-item {:text "Build" :on-action (put-ui-event-fn {:type :build})})
                                               (fx/menu-item {:text "Run" :on-action (put-ui-event-fn {:type :run})})
                                               (fx/menu-item {:text "Build and run" :on-action (put-ui-event-fn {:type :build-run})})
                                               )
                                      (fx/menu {:text "File"}
                                               (fx/menu-item {:text "Save" :on-action (put-ui-event-fn {:type :save-file})})))
                                (VBox/setVgrow Priority/NEVER))
                              (doto (fx/split-pane {:divider-positions (double-array [0.25])
                                                    :focus-traversable true}
                                                   file-menu-container
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
    [main-screen file-menu-container content-container]))

(deftype MainScreen [widget file-menu-container content-container app-requests])

(defn make-main-screen [>app-requests]
  (let [[main-screen file-menu-container content-container] (build-main-screen >app-requests)]
    (->MainScreen main-screen file-menu-container content-container >app-requests)))

(defn set-file-menu-data! [^MainScreen screen file-view-model]
  (gui/set-widget-children (.file_menu_container screen)
                           (list (build-file-menu-widget file-view-model (.app_requests screen)))))

(defn set-content-widget! [^MainScreen screen widget]
  (.setContent (.content_container screen)
               widget))

(defn get-widget [^MainScreen screen]
  (.widget screen))

(defn open-project-dialog [stage]
  (.showDialog
    (doto (DirectoryChooser.)
      (.setTitle "Select project.edn file")
      )
    stage))

