(ns urdmi.gui.empty
  (:require [urdmi.gui-util :as gui]
            [urdmi.core :as core])
  (:import (javafx.scene.layout Pane)))

(extend-type Pane
  core/ContentPage
  (container-node [this]
    this)
  (show-data [this project key modified]
    nil)
  (read-data [this]
    nil))

(def empty-page
  (Pane.))