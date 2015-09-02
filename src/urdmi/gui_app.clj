(ns urdmi.gui-app
  (:require [clojure.core.async :refer [chan go <! >!]]
            [urdmi.core :as core]
            [clojure.zip :as zip]
            [urdmi.gui.relation :as relation-gui]
            [urdmi.gui.code-editor :as code-editor-gui]
            [urdmi.gui.main :as main-gui]
            [urdmi.prolog :as prolog]
            [urdmi.app :as app]
            [fx-clj.core :as fx]
            [me.raynes.fs :as fs]
            [urdmi.gui :as gui])
  (:import (urdmi.core Project)
           (java.io StringWriter)
           (javafx.scene.layout Pane)
           (javafx.scene Scene)
           (javafx.stage Stage)))

(defn- unwrap-urdmi-edit [ast]
  (when (and (= (:type ast) :ast-functor) (= "urdmi_edit" (:name (first (:children ast)))))
    (:name (second (:children ast)))))

(defn- rel-ast-to-table [rel-asts]
  (let [op-manager (:op-manager (prolog/parser-context []))]
    (->> rel-asts
         (mapv (fn [ast]
                 (->> ast
                      :children
                      rest
                      (mapv (fn [ast]
                              (if-let [unwrapped (unwrap-urdmi-edit ast)]
                                unwrapped
                                (let [writer (StringWriter.)]
                                  (prolog/pretty-print ast op-manager writer)
                                  (.toString writer)))))))))))

(defn relations-model-to-viewmodel [rel]
  (let [rel-asts (:ast rel)
        [rel-name rel-arity] (:rel rel)]
    {:name  rel-name
     :arity rel-arity
     :items (rel-ast-to-table rel-asts)}
    ))

(defn relations-viewmodel-to-model [parser-context viewmodel]
  {:rel [(:name viewmodel) (:arity viewmodel)]
   :ast (let [items (:items viewmodel)
              head {:type :ast-atom :name (:name viewmodel)}]
          (for [row items]
            {:type     :ast-functor
             :children (doall
                         (cons head
                               (for [item row
                                     :let [term (prolog/parse-single-term parser-context item)]]
                                 (if term
                                   term
                                   {:type     :ast-functor
                                    :children (list {:type :ast-atom :name "urdmi_edit"} {:type :ast-atom :name item})}))))}))})

(defn- file-names-recursively [zipiter proj-key]
  (let [path (into [proj-key] (conj (mapv :name (rest (zip/path zipiter))) (:name (zip/node zipiter))))
        name (:name (zip/node zipiter))]
    (if (zip/branch? zipiter)
      (vector
        (keep identity (cons [:name (:name (zip/node zipiter)) :path path]
                             (if-let [childiter (zip/down zipiter)]
                               (loop [iter childiter
                                      children []
                                      ]
                                 (if-not (nil? iter)
                                   (recur (zip/right iter) (conj children (file-names-recursively iter proj-key)))
                                   children))

                               (list)
                               ))))
      {:name name :path path})))

(defn- get-file-names [^Project p proj-key display-name]
  (vec (cons {:name display-name :path [proj-key]}
             (rest (first (file-names-recursively (core/file-model-zipper (get-in p (core/dir-keys proj-key))) proj-key))))))

(defn generate-menu-viewmodel [^Project p]
  (let [
        relations (get-file-names p core/relations-keyname "Relations")
        working-dir (get-file-names p core/workdir-keyname "Working dir")
        outputs (get-file-names p core/output-keyname "Output")
        additions (get-file-names p core/additions-keyname "Additions")
        settings (get-file-names p core/settings-keyname "Settings")
        ]
    (flatten [{:name "Project" :path []} relations working-dir outputs additions settings])))

(deftype RelationPage [widget data-key parser-context]
  gui/ContentPage
  (container-node [this]
    (gui/get-node widget))
  (show-data [this project]
    (let [rel-view-model (relations-model-to-viewmodel (get-in project (apply core/dir-keys data-key)))]
      (fx/run! (gui/set-data! widget rel-view-model))))
  (read-data [this]
    (relations-viewmodel-to-model parser-context (gui/get-data widget))
    ))

(defn make-relation-page [data-key]
  (let [parser-context (prolog/parser-context [])]
    (->RelationPage (relation-gui/make-widget parser-context) data-key parser-context)))

(deftype CodeEditorPage [widget data-key]
  gui/ContentPage
  (container-node [this]
    (gui/get-node widget))
  (show-data [this project]
    (gui/set-data! widget @(:text (get-in project (apply core/dir-keys data-key)))))
  (read-data [this]
    (gui/get-data widget)
    ))

(defn make-code-editor-page [data-key]
  (->CodeEditorPage (code-editor-gui/make-widget) data-key))

(extend-type Pane
  gui/ContentPage
  (container-node [this]
    this)
  (show-data [this project]
    nil)
  (read-data [this]
    nil))

(def empty-page
  (Pane.))

(defn make-relation-list-page []
  empty-page)

;cascading dispatch
(defmulti generate-page (fn [cascade-key orig-key app]
                          cascade-key))

(defmethod generate-page [:relations] [cascade-key orig-key app]
  (if (= cascade-key orig-key)
    (make-relation-list-page)
    (make-relation-page orig-key)))

(defmethod generate-page :default [cascade-key orig-key app]
  (generate-page (vec (butlast cascade-key)) orig-key app))

(defmethod generate-page [] [cascade-key orig-key app]
  (let [proj (:project app)
        data (get-in proj (apply core/dir-keys orig-key))]
    (if-not (:text data)
      empty-page
      (make-code-editor-page orig-key)
      )))

(defn init-app [stage]
  (let [app (app/init-app)
        pages {}
        ui-requests (chan)]
    (-> app
        (assoc :stage stage)
        (assoc :pages pages)
        (assoc :ui-requests ui-requests)
        (assoc :main-screen (main-gui/make-main-screen ui-requests))
        )))

(defn load-project [app dir]
  (let [app (app/load-project app dir)
        proj (:project app)
        files-view-model (generate-menu-viewmodel proj)]
    (fx/run! (main-gui/set-menu-files! (:main-screen app) files-view-model))
    app))

(defn switch-page [app key]
  (let [page (get-in app [:pages key :page] nil)
        page (if page page
                      (doto
                        (generate-page key key app)
                        (gui/show-data (:project app))))]
    (fx/run! (main-gui/set-content-widget! (:main-screen app) (gui/container-node page)))
    (-> app
        (assoc-in [:pages key :page] page)
        (assoc :current-page-key key))))

(defmulti handle-request (fn [{:keys [type data]} app]
                           type))

(defmethod handle-request :switch-page [{:keys [type target]} app]
  (switch-page app target))

(defmethod handle-request :modified-page [{:keys [type]} app]
  (let [page-key (:current-page-key app)]
    (fx/run! (main-gui/mark-file-modified! (:main-screen app) page-key))
    (-> app
        (assoc-in [:pages page-key :modified] true)
        )))

(defmethod handle-request :save-page [{:keys [type]} app]
  (let [page-key (:current-page-key app)
        page (get-in app [:pages page-key :page])
        page-data (gui/read-data page)]
    (-> app
        (assoc-in [:pages page-key :modified] false)
        )))

(defmethod handle-request :open-project [event app]
  (if-let [location (fx/run<!! (main-gui/open-project-dialog (:stage app) (fs/file ".")))]
    (-> app
        (load-project location)
        (dissoc :pages)
        (switch-page []))
    app))

(defmethod handle-request :build [event app]
  (app/build-working-dir (:project app))
  )

(defmethod handle-request :run [event app]
  (app/run-learning (:project app))
  )

(defmethod handle-request :build-run [event app]
  (app/build-working-dir (:project app))
  (app/run-learning (:project app))
  )

(defn main-scene [stage]
  (let [app (->
              (init-app stage))
        requests-chan (:ui-requests app)]
    (go
      (loop [app app]
        (recur (handle-request (<! requests-chan) app))))
    (Scene. (main-gui/get-widget (:main-screen app)))))


(defn show-on-test-stage [show-fn]
  (fx/run!
    (let [^Stage stage (fx/stage)]
      (.setScene stage (show-fn stage))
      (.show stage))))

(show-on-test-stage #'main-scene)