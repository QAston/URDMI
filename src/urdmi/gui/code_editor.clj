(ns urdmi.gui.code-editor
  (:require [clojure.core.async :refer [chan go <! >!]]
            [fx-clj.core :as fx]
            [clojure.string :as string]
            [urdmi.util :as util]
            [urdmi.gui :as gui])
  (:import [org.fxmisc.richtext CodeArea LineNumberFactory]
           (javafx.scene.control ContextMenu)
           (javafx.scene.input KeyCode KeyCodeCombination)
           (javafx.beans.binding BooleanExpression)))


; https://github.com/TomasMikula/RichTextFX CodeArea

(defn build-context-menu[^CodeArea code-area]
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
        (add (let [undo-item (fx/menu-item {:disable true
                                            :text        "Undo"
                                       :accelerator (KeyCodeCombination. KeyCode/Z (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                                       :on-action   (fn [e]
                                                      (.undo code-area))})]
               (.bind (.disableProperty undo-item) (.not (BooleanExpression/booleanExpression (.undoAvailableProperty code-area))))
               undo-item
               )))
    (.. getItems
        (add (let [redo-item (fx/menu-item {:disable true
                                            :text        "Redo"
                                            :accelerator (KeyCodeCombination. KeyCode/Y (into-array (list KeyCodeCombination/SHORTCUT_DOWN)))
                                            :on-action   (fn [e]
                                                           (.undo code-area))})]
               (.bind (.disableProperty redo-item) (.not (BooleanExpression/booleanExpression (.redoAvailableProperty code-area))))
               redo-item
               )))))

(defn build-code-editor []
  (let [code-area (CodeArea.)]
    (.setParagraphGraphicFactory code-area (LineNumberFactory/get code-area))
    (.setContextMenu code-area (build-context-menu code-area))
    code-area))

(deftype CodeEditorView [^CodeArea widget]
  gui/View
  (main-widget [this]
    widget)
  (update-widget [this data]
    (.replaceText widget data))
  (read-data [this]
    (.getValue (.textProperty widget))))

(defn make-view []
  (->CodeEditorView (build-code-editor)))

(comment
  (defn test-fn[]
    (let [view (make-view)
          data "teststring"]
      (gui/update-widget view data)
      (println (= (gui/read-data view) data))
      (gui/main-widget view))
    )
  (fx/sandbox #'test-fn))