(ns urdmi.gui
  (:require [clojure.core.async :refer [chan go <! >!]]
            [clojure.java.io :as io]
            [fx-clj.core :as fx]))

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
(comment (load-fxml "main.fxml"))

(defn set-widget-children [node children]
  (fx/run!
    (.. node
        (getChildren)
        (setAll
          children))))



