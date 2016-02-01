(ns urdmi.plugin.aleph.theory-page
  (:require [fx-clj.core :as fx]
            [urdmi.gui-util :as gui]
            [urdmi.core :as core]
            [clojure.string :as str])
  (:import (javafx.scene.control ScrollPane)
           (javafx.scene.layout Pane VBox Priority GridPane ColumnConstraints)
           (javafx.geometry Pos HPos)
           (javafx.scene.paint Color)))

(defn make-test-matrix [test-results]
  (let [{:keys [pos-egz-true neg-egz-true pos-egz-false neg-egz-false]} test-results
        good-total (+ pos-egz-true neg-egz-false)
        sum-total (+ pos-egz-false neg-egz-false pos-egz-true neg-egz-true)]
    (doto (GridPane.)
     (.setHgap 10.0)
     (.setVgap 10.0)
     #_(.. getColumnConstraints
           (setAll [(doto (ColumnConstraints.)
                      (.setHalignment HPos/LEFT))
                    (doto (ColumnConstraints.)
                      (.setHalignment HPos/RIGHT))
                    ]))
     (.add (fx/label {:text "Theory" :rotate -90.0 }) 0 2 1 3)
     (.add (fx/label {:text "Examples"}) 2 0 3 1)

     (.add (fx/label {:text "Pos"}) 1 2 )
     (.add (fx/label {:text "Neg"}) 1 3 )
     (.add (fx/label {:text "Sum"}) 1 4 )
     (.add (fx/label {:text "Pos"}) 2 1 )
     (.add (fx/label {:text "Neg"}) 3 1 )
     (.add (fx/label {:text "Sum"}) 4 1 )
     (.add (fx/label {:text (str pos-egz-true) :text-fill Color/GREEN }) 2 2)
     (.add (fx/label {:text (str neg-egz-true) :text-fill Color/RED }) 3 2)
     (.add (fx/label {:text (str pos-egz-false) :text-fill Color/RED }) 2 3)
     (.add (fx/label {:text (str neg-egz-false) :text-fill Color/GREEN }) 3 3)
     (.add (fx/label {:text (str (+ pos-egz-true neg-egz-true))}) 4 2)
     (.add (fx/label {:text (str (+ pos-egz-true pos-egz-false))}) 2 4)
     (.add (fx/label {:text (str (+ neg-egz-true neg-egz-false))}) 3 4)
     (.add (fx/label {:text (str (+ pos-egz-false neg-egz-false))}) 4 3)
     (.add (fx/label {:text (str (+ pos-egz-false neg-egz-false pos-egz-true neg-egz-true))}) 4 4)
     (.add (fx/label {:text (str "Accuracy: " good-total "/" sum-total " = " (/ (float good-total) (float sum-total)) )}) 0 5 5 1)
     ))
  )

;(fx/sandbox (fn [] (make-test-matrix {:pos-egz-true 12, :neg-egz-true 0, :pos-egz-false 5, :neg-egz-false 17})))

(defn make-theory-view [theory]
  (let [text (str/join "\n\n"
                       (->> theory
                            (map (fn [{:keys [pos-covered neg-covered text]}]
                                   (str "% Examples covered: positive - " pos-covered ", negatove - " neg-covered "\n" text)))))]
    (doto (fx/text-area {:editable       false
                         :max-height     Double/MAX_VALUE
                         :pref-row-count (count (str/split-lines text))})
      (VBox/setVgrow Priority/ALWAYS)
     (.appendText text)))
  )

(deftype HypothesisPage [widget theory-data]
  core/ContentPage
  (container-node [this]
    widget)
  (show-data [this project key modified]
    (when modified
      (let [data (core/get-output-data project (last key))]
        (reset! theory-data data)
        (.. widget getChildren clear)
        (.. widget getChildren (add (fx/label {:text "Generated theory:"})))
        (.. widget getChildren (add (make-theory-view (:theory data))))
        (.. widget getChildren (add (fx/label {:text "Coverage:"})))
        (.. widget getChildren (add (make-test-matrix (:training-results data))))

        )
      )
    )
  (read-data [this]
    (core/file-item @theory-data)
    ))

(defn make-page []
  (->HypothesisPage (fx/v-box {}) (atom nil))
  )

