(ns urdmi.plugin.ace.gui
  (:require [urdmi.plugin.ace.core :as ace]
            [urdmi.gui-util :as gui]
            [urdmi.plugin.ace.settings-page :as settings-page]
            [urdmi.core :as core])
  (:import (urdmi.plugin.ace.core AcePlugin)
           (urdmi.core ContentPage)))

(extend-type AcePlugin
  core/PluginGui
  (new-page ^ContentPage [this project key >ui-requests]
    (condp = key
      [:settings ace/settings-filename] (settings-page/make-page >ui-requests project)
      nil)
    ))