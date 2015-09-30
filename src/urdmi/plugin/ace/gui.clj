(ns urdmi.plugin.ace.gui
  (:require [urdmi.plugin.ace.core :as ace]
            [urdmi.gui :as gui]
            [urdmi.plugin.ace.settings-page :as settings-page])
  (:import (urdmi.plugin.ace.core AcePlugin)
           (urdmi.gui ContentPage)))

(extend-type AcePlugin
  gui/PluginGui
  (new-page ^ContentPage [this project key >ui-requests]
    (condp = key
      [:settings ace/settings-filename] (settings-page/make-page >ui-requests project)
      nil)
    ))