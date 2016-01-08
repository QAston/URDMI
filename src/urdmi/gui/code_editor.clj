(ns urdmi.gui.code-editor
  (:require [clojure.core.async :refer [put! chan go <! >!]]
            [fx-clj.core :as fx]
            [clojure.string :as string]
            [urdmi.util :as util]
            [urdmi.gui-util :as gui]
            [urdmi.core :as core])
  (:import [org.fxmisc.richtext CodeArea LineNumberFactory]
           (javafx.scene.control ContextMenu)
           (javafx.scene.input KeyCode KeyCodeCombination)
           (javafx.beans.binding BooleanExpression)))


; https://github.com/TomasMikula/RichTextFX CodeArea

(defn build-context-menu [^CodeArea code-area]
  (doto (ContextMenu.)
    (.. getItems
        (add (fx/menu-item {:text        "Cut"
                            :accelerator (KeyCodeCombination. KeyCode/X (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                            :on-action   (fn [e]
                                           (.cut code-area))})))
    (.. getItems
        (add (fx/menu-item {:text        "Copy"
                            :accelerator (KeyCodeCombination. KeyCode/C (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                            :on-action   (fn [e]
                                           (.copy code-area))})))
    (.. getItems
        (add (fx/menu-item {:text        "Paste"
                            :accelerator (KeyCodeCombination. KeyCode/V (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                            :on-action   (fn [e]
                                           (.paste code-area))})))
    (.. getItems
        (add (let [undo-item (fx/menu-item {:disable     true
                                            :text        "Undo"
                                            :accelerator (KeyCodeCombination. KeyCode/Z (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                                            :on-action   (fn [e]
                                                           (.undo code-area))})]
               (.bind (.disableProperty undo-item) (.not (BooleanExpression/booleanExpression (.undoAvailableProperty code-area))))
               undo-item
               )))
    (.. getItems
        (add (let [redo-item (fx/menu-item {:disable     true
                                            :text        "Redo"
                                            :accelerator (KeyCodeCombination. KeyCode/Y (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                                            :on-action   (fn [e]
                                                           (.redo code-area))})]
               (.bind (.disableProperty redo-item) (.not (BooleanExpression/booleanExpression (.redoAvailableProperty code-area))))
               redo-item
               )))))

(defn build-code-editor []
  (let [code-area (CodeArea.)]
    (.setParagraphGraphicFactory code-area (LineNumberFactory/get code-area))
    (.setContextMenu code-area (build-context-menu code-area))
    code-area))

(defn- register-data-change-listeners [>ui-requests text-property user-input]
    (gui/on-changed text-property
                    (fn [obs old new]
                      (if @user-input
                        (put! >ui-requests {:type :modified-page})))))

(deftype CodeEditorWidget [^CodeArea widget user-input first]
  gui/DataWidget
  (get-node [this]
    widget)
  (set-data! [this data data-key]
    (reset! user-input false)

    (.replaceText widget data)
    (when @first
      (reset! first false)
      (doto (.getUndoManager widget)
        (.forgetHistory))
      )
    (reset! user-input true))
  (get-data [this]
    (.getValue (.textProperty widget))))

(defn make-widget [>ui-requests]
  (let [widget (build-code-editor)
        text-property (.textProperty widget)
        user-input (atom false)]
    (register-data-change-listeners >ui-requests text-property user-input)
    (->CodeEditorWidget widget user-input (atom true))))

(deftype CodeEditorPage [widget]
  core/ContentPage
  (container-node [this]
    (gui/get-node widget))
  (show-data [this project data-key modified]
    (when modified
      (gui/set-data! widget @(:data (get-in project (apply core/model-map-keys data-key))) data-key)))
  (read-data [this]
    (core/file-item (gui/get-data widget)))
    )

(defn make-page [ui-requests]
  (->CodeEditorPage (make-widget ui-requests)))

(comment
  (defn test-fn []
    (let [view (make-widget (chan))
          data "teststring"]
      (gui/set-data! view data nil)
      (println (= (gui/get-data view) data))
      (gui/get-node view))
    )
  (fx/sandbox #'test-fn))