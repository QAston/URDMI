(ns urdmi.gui.empty
  (:require [urdmi.gui :as gui])
  (:import (javafx.scene.layout Pane)))

(extend-type Pane
  gui/ContentPage
  (container-node [this]
    this)
  (show-data [this project key]
    nil)
  (read-data [this]
    nil))

(def empty-page
  (Pane.))