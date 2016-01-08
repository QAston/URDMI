(ns urdmi.plugin.aleph.gui
  (:require [urdmi.plugin.aleph.core :as aleph]
            [urdmi.gui-util :as gui]
            [urdmi.plugin.aleph.settings-page :as settings-page]
            [urdmi.plugin.aleph.hypothesis-page :as hypothesis-page]
            [urdmi.plugin.aleph.datamining-page :as datamining-page]
            [urdmi.core :as core])
  (:import (urdmi.plugin.aleph.core AlephPlugin)
           (urdmi.core ContentPage)))

(extend-type AlephPlugin
  core/PluginGui
  (new-page ^ContentPage [this project key >ui-requests]
    (condp = key
      [:settings aleph/settings-filename] (settings-page/make-page >ui-requests project)
      [:settings aleph/hypothesis-name] (hypothesis-page/make-page >ui-requests project)
      [:settings aleph/datamining-name] (datamining-page/make-page >ui-requests project)
      nil)
    ))