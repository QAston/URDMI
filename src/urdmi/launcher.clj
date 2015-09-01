(ns urdmi.launcher
  (:gen-class
    :extends javafx.application.Application)
  (:import (javafx.application Application)
           (javafx.scene Scene)))

(defn -main []
  (Application/launch urdmi.launcher (into-array String *command-line-args*))
  )

(defn -start
  "Implements javafx.application.Application.start(javafx.stage.Stage)."
  [app ^javafx.stage.Stage stage]
  (require 'urdmi.gui)
  (let [ args (into-array String (-> app .getParameters .getRaw))
        main-scene ((var-get (resolve (symbol "urdmi.gui-app" "main-scene"))) stage)]
    (doto stage
      (.setScene main-scene)
      (.show))))