(ns urdmi.gui-app
  (:use clojure.core.incubator)
  (:require [clojure.core.async :refer [chan go <! >! alt!]]
            [urdmi.core :as core]
            [clojure.zip :as zip]
            [urdmi.gui.relation :as relation-gui]
            [urdmi.gui.code-editor :as code-editor-gui]
            [urdmi.gui.main :as main-gui]
            [urdmi.prolog :as prolog]
            [urdmi.app :as app]
            [fx-clj.core :as fx]
            [me.raynes.fs :as fs]
            [urdmi.gui :as gui]
            [urdmi.watch-fs :as watch-fs])
  (:import (urdmi.core Project)
           (java.io StringWriter)
           (javafx.scene.layout Pane)
           (javafx.scene Scene)
           (javafx.stage Stage)))

(defn- unwrap-urdmi-edit [ast]
  (when (and (= (:type ast) :ast-functor) (= "urdmi_edit" (:name (first (:children ast)))))
    (:name (second (:children ast)))))

(defn- rel-ast-to-table [parser-context rel-asts]
  (let [op-manager (:op-manager parser-context)]
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

(defn relations-model-to-viewmodel [parser-context rel]
  (let [rel-asts (:ast rel)
        [rel-name rel-arity] (:rel rel)]
    {:name  rel-name
     :arity rel-arity
     :items (rel-ast-to-table parser-context rel-asts)}
    ))

(defn relations-viewmodel-to-model [parser-context viewmodel]
  {:name (str (:name viewmodel) "_" (:arity viewmodel) ".pl")
   :rel  [(:name viewmodel) (:arity viewmodel)]
   :ast  (let [items (:items viewmodel)
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

(defn generate-menu-viewmodel [^Project p]
  (let [vm (for [file (app/get-model-file-keys p true)]
             (case file
               [:working-dir] {:name "Working dir" :path file}
               [:additions] {:name "Additions" :path file}
               [:output] {:name "Output" :path file}
               [:relations] {:name "Relations" :path file}
               [:settings] {:name "Settings" :path file}
               {:name (last file) :path file})
             )]
    (into [{:name "Project" :path []}] vm)))

(deftype RelationPage [widget parser-context]
  gui/ContentPage
  (container-node [this]
    (gui/get-node widget))
  (show-data [this project data-key]
    (let [rel-view-model (relations-model-to-viewmodel parser-context
                                                       (get-in project (apply core/dir-keys data-key)))]
      (fx/run! (gui/set-data! widget rel-view-model data-key))))
  (read-data [this]
    (relations-viewmodel-to-model parser-context (gui/get-data widget))
    ))

(defn make-relation-page [app]
  (let [parser-context (app/plugin-parser-context app)]
    (->RelationPage (relation-gui/make-widget parser-context (:ui-requests app)) parser-context)))

(deftype CodeEditorPage [widget]
  gui/ContentPage
  (container-node [this]
    (gui/get-node widget))
  (show-data [this project data-key]
    (gui/set-data! widget @(:text (get-in project (apply core/dir-keys data-key))) data-key))
  (read-data [this]
    {:text (delay (gui/get-data widget))}
    ))

(defn make-code-editor-page [app]
  (->CodeEditorPage (code-editor-gui/make-widget (:ui-requests app))))

(extend-type Pane
  gui/ContentPage
  (container-node [this]
    this)
  (show-data [this project key]
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
    (make-relation-page app)))

(defmethod generate-page :default [cascade-key orig-key app]
  (generate-page (vec (butlast cascade-key)) orig-key app))

(defmethod generate-page [] [cascade-key orig-key app]
  (let [proj (:project app)
        data (get-in proj (apply core/dir-keys orig-key))]
    (if-not (:text data)
      empty-page
      (make-code-editor-page app)
      )))

(defn init-app [stage]
  (let [app (app/init-app)
        pages {}
        ui-requests (chan)]
    (-> app
        (assoc :stage stage)
        (assoc :pages pages)
        (assoc :ui-requests ui-requests)
        (assoc :fs-changes (chan))
        (assoc :main-screen (main-gui/make-main-screen ui-requests))
        )))

(defn update-gui-for-modified [app]
  (let [modified (get-in app [:pages (:current-page-key app) :modified] false)]
    (main-gui/set-menu-item-enabled! (:main-screen app) :save-file modified)
    (main-gui/set-menu-item-enabled! (:main-screen app) :revert-file modified)))

(defn switch-page [app key]
  (let [page-data (get-in app [:pages key] nil)
        page-data (if page-data
                    page-data
                    (do
                      {:page     (doto
                                   (generate-page key key app)
                                   (gui/show-data (:project app) key))
                       :modified false}))
        app (-> app
                (assoc-in [:pages key] page-data)
                (assoc :current-page-key key))]
    (fx/run! (main-gui/set-content-widget! (:main-screen app) (gui/container-node (:page page-data)))
             (update-gui-for-modified app))
    app))

(defn load-project [app dir]
  (let [app (switch-page app [])
        app (app/load-project app dir)
        proj (:project app)
        files-view-model (generate-menu-viewmodel proj)]
    (fx/run! (main-gui/set-menu-files! (:main-screen app) files-view-model)
             (doseq [menu [:build :build-run :run]]
               (main-gui/set-menu-item-enabled! (:main-screen app) menu true)))
    (when-let [c (get app :fs-changes)]
      (watch-fs/close! c))
    (-> app
        (assoc :fs-changes (watch-fs/changes-chan (app/get-model-dirs (:project app))))
        (dissoc :pages)
        (switch-page []))))

(defmulti handle-request (fn [{:keys [type data]} app]
                           type))

(defmethod handle-request :switch-page [{:keys [type target]} app]
  (switch-page app target))

(defmethod handle-request :modified-page [{:keys [type data-key]} app]
  (let [app (assoc-in app [:pages data-key :modified] true)]
    (fx/run! (main-gui/update-file-viewmodel! (:main-screen app) data-key #(assoc % :modified true))
             (update-gui-for-modified app))
    app))

(defmethod handle-request :revert-file [{:keys [type]} app]
  (let [page-key (:current-page-key app)
        app (assoc-in app [:pages page-key :modified] false)]
    (gui/show-data (get-in app [:pages page-key :page]) (:project app) page-key)
    (fx/run! (main-gui/update-file-viewmodel! (:main-screen app) page-key #(assoc % :modified false))
             (update-gui-for-modified app))
    app))

(defmethod handle-request :save-file [{:keys [type]} app]
  (let [page-key (:current-page-key app)
        app-page (get-in app [:pages page-key])
        page (:page app-page)
        old-page-data (get-in (:project app) (apply core/dir-keys page-key))
        page-data (merge old-page-data
                         (gui/read-data page))
        new-page-key (conj (vec (butlast page-key)) (:name page-data))
        proj (-> (:project app)
                 (dissoc-in (apply core/dir-keys page-key))
                 (assoc-in (apply core/dir-keys new-page-key) page-data))
        app (-> app
                (dissoc-in [:pages page-key])
                (assoc-in [:pages page-key] (-> app-page
                                                (assoc :modified false)))
                (assoc :project proj)
                ;save is before delete, so that on crash the file is preserved
                (app/save-model-to-file new-page-key)
                )
        ]
    (fx/run!
      (main-gui/update-file-viewmodel!
        (:main-screen app) page-key #(-> %
                                         (assoc :modified false)
                                         (assoc :path new-page-key)
                                         (assoc :name (last new-page-key)))))
    ; delete file if renamed
    (when-not (= page-key new-page-key)
      (fs/delete (core/name-keys-to-file proj page-key))
      ; todo:
      ; remove the need of tracing the key by the view
      ; give it ref managed by framework, or add page-key to ui-requests from framework
      ;todo:
      ;disallow renaming file to an already existing one!
      (gui/show-data page (:project app) new-page-key))
    app))

(defmethod handle-request :open-project [event app]
  (if-let [location (fx/run<!! (main-gui/open-project-dialog (:stage app) (fs/file ".")))]
    (load-project app location)
    app))

(defmethod handle-request :build [event app]
  (app/build-working-dir (:project app))
  app
  )

(defmethod handle-request :run [event app]
  (println (app/run-learning (:project app)))
  app
  )

(defmethod handle-request :build-run [event app]
  (app/build-working-dir (:project app))
  (app/run-learning (:project app))
  app
  )

(defmulti handle-fs-change (fn [[event file time] app]
                             event))

(defmethod handle-fs-change :create [[event file time] app]
  (let [file-key (core/file-to-name-keys (:project app) (fs/file file))]
    (if (get-in (:project app) (apply core/dir-keys file-key))
      app
      (do
        (fx/run!
          (main-gui/add-menu-files! (:main-screen app) (list {:name (last file-key) :path file-key})))
        (app/load-file-to-model app file)
        ))))

(defmethod handle-fs-change :modify [[event file time] app]
  ;todo: check if file modified
  (if (and (fs/exists? file) (get-in (:project app) (apply core/dir-keys (core/file-to-name-keys (:project app) file))))
    (let [{:keys [app needs-sync]} (app/update-fs-sync-status app
                                                              (core/file-to-name-keys (:project app) file)
                                                              time)]
      (if needs-sync
        (app/load-file-to-model app file)
        app
        ))
    app))

(defmethod handle-fs-change :delete [[event file time] app]
  (let [file-key (core/file-to-name-keys (:project app) (fs/file file))]
    (if-not (get-in (:project app) (apply core/dir-keys file-key))
      app
      (do
        (fx/run!
          (main-gui/remove-file! (:main-screen app) file-key))
        (->
          (app/delete-file-from-model app file)
          (dissoc-in [:pages file-key]))
        ))))

(defn main-scene [stage]
  (let [app (->
              (init-app stage)
              ;(load-project (fs/file "dev-resources/projects/aleph_default/"))
              ;(load-project (fs/file "dev-resources/projects/ace_tilde/"))
              )
        ]
    (go
      (loop [app app]
        (recur
          (alt! (:ui-requests app) ([ui-request]
                                     (handle-request ui-request app))
                (:fs-changes app) ([change]
                                    (handle-fs-change change app))))))

    (Scene. (main-gui/get-widget (:main-screen app)))))


(defn show-on-test-stage [show-fn]
  (fx/run!
    (let [^Stage stage (fx/stage)]
      (.setScene stage (show-fn stage))
      (.show stage))))

(show-on-test-stage #'main-scene)