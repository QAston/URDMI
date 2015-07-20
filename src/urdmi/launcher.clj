(ns urdmi.launcher
  (:gen-class
    :extends javafx.application.Application)
  (:import (javafx.application Application)
           (javafx.scene Scene)
           (urdmi launcher)))

#_(defn launch [start-fn]
  (def start start-fn)
  (Application/launch your_class_name (into-array String *command-line-args*)))
#_(defn -start [_ stage]
  (start stage))

#_(defn sandbox
  "Creates a JavaFX stage with the root element of the stage's scene set to
  the result of evaluating refresh-fn. If F5 is pressed within the stage,
  refresh-fn will be re-evaluated and its new result will be bound to as the
  root of the scene. This can be very useful for prototyping.

  Suggested usage:

  (defn my-refresh-fn [] (do-create-view....))
  (sandbox #'my-refresh-fn)
  ;; By binding to a var,  my-refresh-fn can be  easily updated and reloaded
  ;; at the REPL"
  [refresh-fn & {:keys [title maximized]
                 :or {title (str "Sandbox" (swap! auto-inc inc))}}]
  (run<!!
    (let [scene (fx/scene (refresh-fn))
          stage (fx/stage)]
      (pset! scene
             {:on-key-pressed
              (fn do-sandbox-refresh [e]
                (when (= KeyCode/F5 (.getCode e))
                  (pset! scene {:root (refresh-fn)})))})
      (.setScene stage scene)
      (.initModality stage Modality/NONE)
      (pset! stage {:title title})
      (when maximized (.setMaximized stage true))
      (.show stage)
      stage)))

(defn -main []
  (Application/launch urdmi.launcher (into-array String *command-line-args*))
  )

(defn -start
  "Implements javafx.application.Application.start(javafx.stage.Stage)."
  [app ^javafx.stage.Stage stage]
  (require 'urdmi.gui)
  (let [ args (into-array String (-> app .getParameters .getRaw))
         main-view (var-get (resolve (symbol "urdmi.gui" "main-view")))]
    ; My app loads "scene" from .fxml here but do whatever to init window here...
    (doto stage
      (.setTitle "Hi I'm a window!")
      (.setScene (Scene. main-view))
      (.show))))