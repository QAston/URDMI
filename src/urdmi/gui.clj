(ns urdmi.gui
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [fx-clj.core :as fx])
  (:import [javafx.beans.property.StringProperty]
           (javafx.scene.layout AnchorPane Region VBox Priority HBox)
           (javafx.geometry Pos Insets)
           (javafx.scene.text Font TextAlignment)
           (javafx.scene.paint Color)))

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

(defn new-main-view []
  (let [font (Font/font 11.0)
        text-fill (Color/color 0.625 0.625 0.625)]
    (fx/v-box {:pref-height 600
               :pref-width  900
               }
              (doto (fx/menu-bar
                      (fx/menu {:text "Project"}
                               (fx/menu-item {:text "New"})
                               (fx/menu-item {:text "Open..."})))
                (VBox/setVgrow Priority/NEVER))
              (doto (fx/split-pane {:divider-positions (double-array [0.25])
                                    :focus-traversable true
                                    :pref-height       Region/USE_COMPUTED_SIZE
                                    :pref-width        Region/USE_COMPUTED_SIZE}
                                   (fx/anchor-pane
                                     (doto (fx/tree-view :#views {:layout-x    11.0
                                                                  :layout-y    14.0
                                                                  :pref-height 200.0
                                                                  :pref-width  200.0})
                                       (AnchorPane/setBottomAnchor 0.0)
                                       (AnchorPane/setLeftAnchor 0.0)
                                       (AnchorPane/setTopAnchor 0.0)
                                       (AnchorPane/setRightAnchor 0.0)
                                       ))
                                   (fx/scroll-pane {}
                                                   (fx/anchor-pane :#Content {:max-height     Region/USE_PREF_SIZE
                                                                              :max-width      Region/USE_PREF_SIZE
                                                                              :min-width      Region/USE_COMPUTED_SIZE
                                                                              :pick-on-bounds false}
                                                                   (fx/label {:alignment      Pos/CENTER
                                                                              :layout-x       14.0
                                                                              :layout-y       14.0
                                                                              :text           "View"
                                                                              :text-alignment TextAlignment/CENTER
                                                                              :wrap-text      false}))))
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
              )))

(def create-main-view new-main-view)

(fx/sandbox #'create-main-view)
(comment
  (fx/sandbox #'create-main-view))



