(ns urdmi.gui-app
  (:require [clojure.core.async :refer [chan go <! >!]]
            [urdmi.core :as core]
            [clojure.zip :as zip]
            [urdmi.gui.relation :as relation-gui]
            [urdmi.gui.main :as main-gui]
            [urdmi.prolog :as prolog]
            [urdmi.app :as app]
            [fx-clj.core :as fx]
            [me.raynes.fs :as fs]
            [urdmi.gui :as gui])
  (:import (urdmi.core Project)
           (java.io StringWriter)))

(defn- rel-ast-to-table [rel-asts]
  (let [op-manager (:op-manager (prolog/parser-context []))]
    (->> rel-asts
         (mapv (fn [ast]
                 (->> ast
                      :children
                      rest
                      (mapv (fn [ast]
                              (let [writer (StringWriter.)]
                                (prolog/pretty-print ast op-manager writer)
                                (.toString writer))))))))))

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
        additions (get-file-names p core/output-keyname "Additions")
        settings (get-file-names p core/output-keyname "Settings")
        ]
    [{:name "Project" :path []} relations working-dir outputs additions settings]))

(deftype RelationPage [widget data-key]
  gui/ContentPage
  (container-node [this]
    (gui/get-node widget))
  (show-data [this project]
    (let [rel-view-model (relations-model-to-viewmodel (get-in project (apply core/dir-keys data-key)))]
      (gui/set-data! widget rel-view-model)))
  (read-data [this]
    (relations-viewmodel-to-model (gui/get-data widget))
    ))

(defn make-relation-page [data-key]
  (->RelationPage (relation-gui/make-widget) data-key))

;cascading dispatch
(defmulti generate-page (fn [cascade-key orig-key app]
                          cascade-key))

(defmethod generate-page [:relations] [cascade-key orig-key app]
  (make-relation-page orig-key))

(defmethod generate-page :default [cascade-key orig-key app]
  (generate-page (vec (butlast cascade-key)) orig-key app))

(defmethod generate-page nil [cascade-key orig-key app]
  nil)

(defn init-app []
  (let [app (app/init-app)
        pages {}
        ui-requests (chan)]
    (-> app
        (assoc :pages pages)
        (assoc :ui-requests ui-requests)
        (assoc :main-screen (main-gui/make-main-screen ui-requests))
        )))

(defn load-project [app dir]
  (let [app (app/load-project app dir)
        proj (:project app)
        files-view-model (generate-menu-viewmodel proj)]
    (main-gui/set-file-menu-data! (:main-screen app) files-view-model)
    app))

(defn get-or-gen-page [app key]
  (get-in app [:pages key] (generate-page key key app)))

(defn switch-page [app key]
  (let [page (get-or-gen-page app key)]
    (gui/show-data page (:project app))
    (main-gui/set-content-widget! (:main-screen app) (gui/container-node page))
    (assoc-in app [:pages key] page)))

(defn test-app []
  (let [app (->
              (init-app)
              (load-project (fs/file "dev-resources/projects/aleph_default/"))
              (switch-page [:relations "dzial_6.pl"]))]
    (main-gui/get-widget (:main-screen app))))

(fx/sandbox #'test-app)