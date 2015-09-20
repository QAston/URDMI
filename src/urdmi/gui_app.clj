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
            [urdmi.gui.dialogs :as dialogs]
            [clojure.stacktrace :as stacktrace]
            [urdmi.watch-fs :as watch-fs])
  (:import (urdmi.core Project)
           (java.io StringWriter File)
           (javafx.scene.layout Pane)
           (javafx.scene Scene)
           (javafx.stage Stage)))

(defn- unwrap-urdmi-edit [ast]
  (when (and (= (:type ast) :ast-functor) (= "urdmi_edit" (:name (first (:children ast)))))
    (:name (second (:children ast)))))

(defn- rel-ast-to-table [parser-context rel-asts]
  (->> rel-asts
       (mapv (fn [ast]
               (->> ast
                    :children
                    rest
                    (mapv (fn [ast]
                            (if-let [unwrapped (unwrap-urdmi-edit ast)]
                              unwrapped
                              (let [writer (StringWriter.)]
                                (prolog/pretty-print ast parser-context writer)
                                (.toString writer))))))))))

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

(defn is-page-modified [app page-key]
  (get-in app [:pages page-key :modified] false))

(defn is-page-modified-or-desynced [app page-key]
  (or (app/is-desynced app page-key) (is-page-modified app page-key)))

(defn update-main-menu-for-current-page! [app]
  (main-gui/set-menu-item-enabled! (:main-screen app) :reload-file (not (app/is-dir app (:current-page-key app))))
  (main-gui/set-menu-item-enabled! (:main-screen app) :save-file (is-page-modified-or-desynced app (:current-page-key app)))
  (main-gui/set-menu-item-enabled! (:main-screen app) :revert-file (is-page-modified app (:current-page-key app)))
  )

(defn update-file-menu-for-page! [app page-key]
  (main-gui/update-file-viewmodel! (:main-screen app) page-key #(assoc % :modified (is-page-modified-or-desynced app page-key)))
  )

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
             (update-main-menu-for-current-page! app))
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
    (fx/run! (update-file-menu-for-page! app data-key)
             (update-main-menu-for-current-page! app))
    app))

(defn revert-model-page [app page-key]
  (let [app (assoc-in app [:pages page-key :modified] false)]
    (gui/show-data (get-in app [:pages page-key :page]) (:project app) page-key)
    (fx/run! (update-file-menu-for-page! app page-key)
             (update-main-menu-for-current-page! app))
    app))

(defn reload-model-page [app key]
  (let [file (core/name-keys-to-file (:project app) key)
        app (app/load-file-to-model app file)]
    (if (get-in app [:pages key])
      (revert-model-page app key)
      app)))

(defmethod handle-request :revert-file [{:keys [type]} app]
  (revert-model-page app (:current-page-key app)))

(defmethod handle-request :reload-file [{:keys [type]} app]
  (reload-model-page app (:current-page-key app)))

(defn save-model-page [app page-key]
  (let [app-page (get-in app [:pages page-key])
        page (:page app-page)
        old-page-data (get-in (:project app) (apply core/dir-keys page-key))
        page-data (merge old-page-data
                         (gui/read-data page))
        new-page-key (conj (vec (butlast page-key)) (:name page-data))
        update-current-page-if-needed (fn [app]
                                        (if (= (:current-page-key app) page-key)
                                          (assoc app :current-page-key new-page-key)
                                          app))

        delete-old-page-if-renamed (fn [app]
                                     (if-not (= page-key new-page-key)
                                       (app/delete-model-page app page-key)
                                       app))
        proj (-> (:project app)
                 (dissoc-in (apply core/dir-keys page-key))
                 (assoc-in (apply core/dir-keys new-page-key) page-data))
        app (-> app
                (dissoc-in [:pages page-key])
                (assoc-in [:pages page-key] (-> app-page
                                                (assoc :modified false)))
                (assoc :project proj)
                (delete-old-page-if-renamed)
                (update-current-page-if-needed)
                ;save is before delete, so that on crash the file is preserved
                (app/save-model-to-file new-page-key)
                )
        ]
    (fx/run!
      (main-gui/update-file-viewmodel!
        (:main-screen app) page-key #(-> %
                                         (assoc :path new-page-key)
                                         (assoc :name (last new-page-key))))
      (update-file-menu-for-page! app new-page-key)
      (update-main-menu-for-current-page! app))
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

(defmethod handle-request :save-file [{:keys [type]} app]
  (let [page-key (:current-page-key app)]
    (save-model-page app page-key)))

(defmethod handle-request :open-project [event app]
  (if-let [location (fx/run<!! (dialogs/open-project (:stage app) (fs/file ".")))]
    (load-project app location)
    app))

(defn file-pages-seq [app]
  (->> app
       (:pages)
       (keys)
       (filter #(and (not (app/is-dir app %)) (not-empty %)))))

(defn unsaved-pages [app]
  (filter #(is-page-modified-or-desynced app %) (file-pages-seq app)))

(defn save-model-pages [app keys]
  (reduce save-model-page app keys))

(defn do-with-saved-project [app operation app-fn]
  (let [unsaved (unsaved-pages app)
        proceed (or (empty? unsaved) (fx/run<!! (dialogs/confirm-saving-all (:stage app) operation)))]
    (if proceed
      (let [app (save-model-pages app unsaved)]
        (app-fn app))
      app)))

(defmethod handle-request :build [event app]
  (do-with-saved-project app "build"
                         (fn [app]
                           (app/build-working-dir (:project app))
                           app)))

(defmethod handle-request :run [event app]
  (do-with-saved-project app "run"
                         (fn [app]
                           (let [run-result (app/run-learning (:project app))]
                             (fx/run! (main-gui/add-dm-log-entry! (:main-screen app) (:out run-result)))
                             app))))

(defmethod handle-request :build-run [event app]
  (do-with-saved-project app "build and run"
                         (fn [app]
                           (app/build-working-dir (:project app))
                           (let [run-result (app/run-learning (:project app))]
                             (fx/run! (main-gui/add-dm-log-entry! (:main-screen app) (:out run-result)))
                             app))))

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

(defn- is-page-modified-or-current [app key]
  (or (= key (:current-page-key app))
      (is-page-modified app key)))

(defn mark-page-desynced [app page-key]
  (let [app (app/mark-file-desynced app page-key)]
    (fx/run!
      (update-main-menu-for-current-page! app)
      (update-file-menu-for-page! app page-key))
    app))

(defmethod handle-fs-change :modify [[event ^File file time] app]
  ; skip events on dirs
  (if (.isDirectory file)
    app
    (let [file-key (core/file-to-name-keys (:project app) file)]
      (if-not (and (fs/exists? file) (get-in (:project app) (apply core/dir-keys file-key)))
        app
        (let [{:keys [app needs-sync]} (app/update-fs-sync-status app file-key time)]
          (if (and
                needs-sync
                (is-page-modified-or-current app (core/file-to-name-keys (:project app) file)))
            (if (fx/run<!! (dialogs/reload-modified-file (:stage app) file))
              (reload-model-page app file-key)
              (mark-page-desynced app file-key))
            app))))))

(defn close-page-if-open [app key]
  (if (= key (:current-page-key app))
    (switch-page app [])
    app))

(defmethod handle-fs-change :delete [[event file time] app]
  (let [file-key (core/file-to-name-keys (:project app) (fs/file file))]
    (if-not (get-in (:project app) (apply core/dir-keys file-key))
      app
      (do
        (fx/run!
          (main-gui/remove-file! (:main-screen app) file-key))
        (-> app
            (close-page-if-open file-key)
            (app/delete-model-page file-key)
            (dissoc-in [:pages file-key]))
        ))))

(defn main-scene [stage]
  (let [app (->
              (init-app stage)
              (load-project (fs/file "dev-resources/projects/aleph_default/"))
              ;(load-project (fs/file "dev-resources/projects/ace_tilde/"))
              )
        ]
    (go
      (loop [app app]
        (recur
          (try
            (alt! (:ui-requests app) ([ui-request]
                                       (handle-request ui-request app))
                  (:fs-changes app) ([change]
                                      (handle-fs-change change app)))
            (catch Exception e
              (let [writer (StringWriter.)]
                (binding [*out* writer]
                  (println "An application error occured:")
                  (stacktrace/print-cause-trace e))
                (main-gui/add-app-log-entry! (:screen app) (str writer)))
              app
              )))))

    (Scene. (main-gui/get-widget (:main-screen app)))))


(defn show-on-test-stage [show-fn]
  (fx/run!
    (let [^Stage stage (fx/stage)]
      (.setScene stage (show-fn stage))
      (.show stage))))

(watch-fs/stop-all-channel-watchers!)
(show-on-test-stage #'main-scene)