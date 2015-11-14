(ns urdmi.plugin.aleph.gui
  (:require [urdmi.plugin.aleph.core :as aleph]
            [urdmi.gui :as gui]
            [urdmi.plugin.aleph.settings-page :as settings-page]
            [urdmi.plugin.aleph.hypothesis-page :as hypothesis-page])
  (:import (urdmi.plugin.aleph.core AlephPlugin)
           (urdmi.gui ContentPage)))

(extend-type AlephPlugin
  gui/PluginGui
  (new-page ^ContentPage [this project key >ui-requests]
    (condp = key
      [:settings aleph/settings-filename] (settings-page/make-page >ui-requests project)
      [:settings aleph/hypothesis-name] (hypothesis-page/make-page >ui-requests project)
      nil)
    ))