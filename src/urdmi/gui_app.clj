(ns urdmi.gui-app
  (:use urdmi.util)
  (:require [clojure.core.async :refer [chan go <! >! alt! put!]]
            [urdmi.core :as core]
            [urdmi.gui.relation :as relation-gui]
            [urdmi.gui.code-editor :as code-editor-gui]
            [urdmi.gui.empty :as empty-gui]
            [urdmi.gui.main :as main-gui]
            [urdmi.app :as app]
            [fx-clj.core :as fx]
            [me.raynes.fs :as fs]
            [urdmi.gui :as gui]
            [urdmi.gui.project-settings :as project-settings-gui]
            [urdmi.gui.dialogs :as dialogs]
            [urdmi.gui.relation-list :as relation-list-gui]
            [clojure.stacktrace :as stacktrace]
            [urdmi.watch-fs :as watch-fs]

            [clojure.core.async :as async]
            [clojure.java.io :as io])
  (:import (urdmi.core Project ModelDiff)
           (java.io StringWriter File)
           (javafx.scene Scene)
           (javafx.stage Stage WindowEvent)
           (java.awt Desktop)
           (javafx.event EventHandler)))

(def instances (atom 0))

(defn generate-menu-viewmodel [^Project p]
  (let [vm (->> (app/get-model-item-keys p true)
                (remove #{[:working-dir] [:prolog-ext] [:output] [:relations] [:settings]})
                (map (fn [key] {:name (last key) :path key})))]
    (into [{:name "Project" :path []}
           {:name "Build dir (Advanced)" :path [:working-dir]}
           {:name "Prolog extensions (Advanced)" :path [:prolog-ext]}
           {:name "Output" :path [:output]}
           {:name "Relations" :path [:relations]}
           {:name "Settings" :path [:settings]}] vm)))

;cascading dispatch
(defmulti generate-page (fn [cascade-key orig-key app]
                          cascade-key))

(defmethod generate-page [:relations] [cascade-key orig-key app]
  (if (= cascade-key orig-key)
    (relation-list-gui/make-page)
    (relation-gui/make-page (:ui-requests app) (app/plugin-parser-context app))))

(defmethod generate-page [:settings "project.edn"] [cascade-key orig-key app]
  (project-settings-gui/make-page (:ui-requests app) (:project app)))

(defmethod generate-page :default [cascade-key orig-key app]
  (generate-page (vec (butlast cascade-key)) orig-key app))

(defmethod generate-page [] [cascade-key orig-key app]
  (let [proj (:project app)
        item (get-in proj (apply core/model-map-keys orig-key))]
    (if-not (and (:data item) (instance? String @(:data item)))
      empty-gui/empty-page
      (code-editor-gui/make-page (:ui-requests app))
      )))

(defmulti model-modified (fn [app old-app cascade-key orig-key]
                           cascade-key))

(defmethod model-modified :default [app old-app cascade-key orig-key]
  (model-modified app old-app (vec (butlast cascade-key)) orig-key))

(declare remove-pages)
(declare initialize-watching-fs)
(declare load-pages)

(defmethod model-modified [:settings "project.edn"] [app old-app cascade-key orig-key]

  (let [old-project (:project old-app)
        project (:project app)]
    (if (not= (core/get-working-dir old-project) (core/get-working-dir project))
      (-> app
          (remove-pages (app/get-model-item-keys-by-key old-project [:working-dir] false))
          (initialize-watching-fs)
          (load-pages (core/dir-seq (:project app) [:working-dir])))
      app)))

(defmethod model-modified [] [app old-app cascade-key orig-key]
  app)

(declare apply-diff-to-pages)
(declare update-file-menu-for-page!)

(defn handle-is-model-invalid [app key]
  (condp = key
    [:settings "project.edn"] (app/validate-settings (:project app))
    (core/is-model-invalid (app/plugin app) (:project app) key)))

(defn validate-model-page [app key]
  (let [app (assoc-in app [:invalid-files key] (handle-is-model-invalid app key))]
    (fx/run<!! (update-file-menu-for-page! app key))
    app))

(defn validate-model [app]
  (reduce validate-model-page app (app/get-model-item-keys (:project app) false)))

(defn is-model-valid [app]
  (every? not (vals (get app :invalid-files))))

(defn get-first-invalid [app]
  (first (first (filter (fn [[k v]]
                          v) (get app :invalid-files)))))

(defn handle-model-modified [app old-app key]
  (let [app (if (app/plugin app)
              (apply-diff-to-pages app (core/model-modified (app/plugin app) (:project app) key))
              app)]
    (-> app
        (model-modified old-app key key)
        (validate-model-page key))))

(defn init-app [stage]
  (let [app (app/init-app)
        pages {}
        invalid-files {}
        ui-requests (chan)]

    (fx/run<!! (.setTitle stage "URDMI"))
    (-> app
        (assoc :stage stage)
        (assoc :pages pages)
        (assoc :invalid-files invalid-files)
        (assoc :ui-requests ui-requests)
        (assoc :fs-changes (chan))
        (assoc :main-screen (main-gui/make-main-screen ui-requests))
        )))

(defn is-page-modified [app page-key]
  (get-in app [:pages page-key :modified] false))

(defn is-page-modified-or-desynced [app page-key]
  (or (app/is-desynced app page-key) (is-page-modified app page-key)))

(defn update-main-menu-for-current-page! [app]
  (let [project-jobs-enabled (and (:project app) (not (:job app)))]
    (doseq [menu [:build :build-run :run :save-project]]
      (main-gui/set-menu-item-enabled! (:main-screen app) menu project-jobs-enabled)))
  (main-gui/set-menu-item-enabled! (:main-screen app) :reload-file (not (app/is-dir app (:current-page-key app))))
  (main-gui/set-menu-item-enabled! (:main-screen app) :save-file (is-page-modified-or-desynced app (:current-page-key app)))
  (main-gui/set-menu-item-enabled! (:main-screen app) :revert-file (is-page-modified app (:current-page-key app)))
  )

(defn update-file-menu-for-page! [app page-key]
  (main-gui/update-file-viewmodel! (:main-screen app) page-key #(-> %
                                                                    (assoc :invalid (get-in app [:invalid-files page-key] nil))
                                                                    (assoc :modified (is-page-modified-or-desynced app page-key))))
  )

(defn sync-page-data [app key]
  (fx/run<!!
    (gui/show-data
      (get-in app [:pages key :page])
      (:project app)
      key
      (get-in app [:pages key :needs-data-sync])))
  (assoc-in app [:pages key :needs-data-sync] false))

(defn mark-page-needs-data-sync [app key]
  (let [app (-> app
                (assoc-in [:pages key :needs-data-sync] true))]
    (if (= (:current-page-key app) key)
      (sync-page-data app key)
      app)))

(defn switch-page [app key]
  (let [page-data (get-in app [:pages key] nil)
        page-data (if page-data
                    page-data
                    (do
                      {:page            (doto
                                          (if-let [plugin-page (when (app/plugin app)
                                                                 (gui/new-page (app/plugin app) (:project app) key (:ui-requests app)))]
                                            plugin-page
                                            (generate-page key key app))
                                          )
                       :modified        false
                       :needs-data-sync true}))
        app (-> app
                (assoc-in [:pages key] page-data)
                (assoc :current-page-key key)
                (sync-page-data key))]
    (fx/run<!! (main-gui/set-content-widget! (:main-screen app) (gui/container-node (:page page-data)))
               (update-main-menu-for-current-page! app))
    app))

(defn stop-current-job [app]
  (if-let [job (get app :job)]
    (let [app (->
                app
                (dissoc :job))]
      (when-not (realized? (:task job))
        (future-cancel (:task job)))
      (fx/run<!! (main-gui/stop-job! (:main-screen app))
                 (update-main-menu-for-current-page! app))
      app)
    app))

(defn start-job [app name job-fn]
  (let [app (assoc app :job {:name name
                             :task (future (let [result (job-fn)]
                                             (if (core/thread-interruped?)
                                               (put! (:ui-requests app)
                                                     {:type :stop-job})
                                               (put! (:ui-requests app)
                                                     {:type   :job-finished
                                                      :result result}))
                                             ))})]
    (fx/run<!! (main-gui/start-job! (:main-screen app) name (:ui-requests app))
               (update-main-menu-for-current-page! app))
    app))

(defn initialize-watching-fs [app]
  (when-let [c (get app :fs-changes)]
    (watch-fs/close! c))
  (-> app
      (assoc :fs-changes (watch-fs/changes-chan (app/get-model-dirs (:project app))))))

(defn change-project [app project-fn]
  (let [app (-> app
                (switch-page [])
                (assoc :pages {})
                (assoc :invalid-files {})
                (stop-current-job)
                (project-fn)
                )
        proj (:project app)
        files-view-model (generate-menu-viewmodel proj)]
    (fx/run<!! (main-gui/set-menu-files! (:main-screen app) files-view-model)
               (update-main-menu-for-current-page! app)
               (.setTitle (:stage app) (str "URDMI - " (:project-dir proj))))
    (-> app
        (validate-model)
        (initialize-watching-fs)
        (switch-page [])
        )))

(defn load-project [app dir]
  (let [apply-diff (fn [app]
                     (if-let [model-diff (core/model-loaded (:plugin (:project app)) (:project app))]
                       (do
                         (doseq [key (:remove model-diff)]
                           (fs/delete-dir (core/item-key-to-file (:project app) key)))
                         (assoc app :project
                                    (core/apply-diff (:project app) model-diff)))
                       app))]
    (change-project app (fn [app]
                          (-> app
                              (app/load-project dir)
                              (apply-diff))))))

(defn create-project [app project-data]
  (let [apply-diff (fn [app]
                     (assoc app :project
                                (core/apply-diff (:project app)
                                                 (core/model-created (:plugin (:project app)) (:project app)))))
        save-all (fn [app]
                   (app/save-files app (app/get-model-item-keys (:project app) true)))]
    (change-project app (fn [app]
                          (-> app
                              (app/new-project (:project-dir project-data) (:plugin project-data))
                              (apply-diff)
                              (save-all))))))

(defmulti handle-request (fn [{:keys [type data]} app]
                           type))

(defmethod handle-request :switch-page [{:keys [type target]} app]
  (switch-page app target))

(defmethod handle-request :modified-page [{:keys [type]} app]
  (let [data-key (:current-page-key app)
        app (assoc-in app [:pages data-key :modified] true)]
    (fx/run<!! (update-file-menu-for-page! app data-key)
               (update-main-menu-for-current-page! app))
    app))

(defn revert-model-page [app page-key]
  (let [app (-> app
                (assoc-in [:pages page-key :modified] false)
                (mark-page-needs-data-sync page-key)
                )]
    (fx/run<!! (update-file-menu-for-page! app page-key)
               (update-main-menu-for-current-page! app))
    app))

(defn reload-model-page [app key]
  (let [old-app app
        file (core/item-key-to-file (:project app) key)
        app (app/load-file-to-model app file)]
    (if (get-in app [:pages key])
      (-> app
          (revert-model-page key)
          (handle-model-modified old-app key))
      app)))

(defn build-working-dir [app]
  (let [p (:project app)]
    (core/rebuild-working-dir (:plugin p) p))
  )

;todo: pass channel/writer/outputstream to the plugin, so it can provide async log updates
; in addition to sync return value
(defn run-learning [app]
  (let [p (:project app)
        result (core/run (:plugin p) p)]
    ; todo: reload working dir here
    (apply-diff-to-pages app (core/generate-output (:plugin p) p result))
    (put! (:ui-requests app) {:type :log-datamining :message (:out result)})
    result))

(defmethod handle-request :revert-file [{:keys [type]} app]
  (revert-model-page app (:current-page-key app)))

(defmethod handle-request :reload-file [{:keys [type]} app]
  (reload-model-page app (:current-page-key app)))

(defn save-model-page [app page-key]
  (let [old-app app
        app-page (get-in app [:pages page-key])
        page (:page app-page)
        old-page-data (get-in (:project app) (apply core/model-map-keys page-key))
        page-data (merge old-page-data
                         (fx/run<!! (gui/read-data page)))
        new-page-key (conj (vec (butlast page-key)) (:name page-data))
        update-current-page-if-needed (fn [app]
                                        (if (= (:current-page-key app) page-key)
                                          (assoc app :current-page-key new-page-key)
                                          app))

        delete-old-page-if-renamed (fn [app]
                                     (if-not (= page-key new-page-key)
                                       (-> app
                                           (app/delete-model-page page-key)
                                           (dissoc-in [:pages page-key])
                                           (handle-model-modified old-app page-key))
                                       app))
        proj (-> (:project app)
                 (dissoc-in (apply core/model-map-keys page-key))
                 (assoc-in (apply core/model-map-keys new-page-key) page-data))
        app (-> app
                (assoc-in [:pages new-page-key]
                          (-> app-page
                              (assoc :modified false)))
                (assoc :project proj)
                (delete-old-page-if-renamed)
                (update-current-page-if-needed)
                ;save is before delete, so that on crash the file is preserved
                (app/save-model-to-file new-page-key)
                (handle-model-modified old-app new-page-key))
        ]
    (fx/run<!!
      (main-gui/update-file-viewmodel!
        (:main-screen app) page-key #(-> %
                                         (assoc :path new-page-key)
                                         (assoc :name (last new-page-key))))
      (update-file-menu-for-page! app new-page-key)
      (update-main-menu-for-current-page! app))
    ; delete file if renamed
    (when-not (= page-key new-page-key)
      ;todo:
      ;disallow renaming file to an already existing one!
      (fs/delete (core/item-key-to-file proj page-key)))
    app))

(defmethod handle-request :save-file [{:keys [type]} app]
  (let [page-key (:current-page-key app)]
    (save-model-page app page-key)))

(defmethod handle-request :open-project [event app]
  (if-let [location (fx/run<!! (let [loc (dialogs/open-project (:stage app) (fs/file "."))]
                                 (if (app/is-project-dir loc)
                                   loc
                                   (do
                                     (when loc
                                       (dialogs/error-alert (:stage app) "Not a project dir!" (str "Location " loc " is not an urdmi project directory!")))
                                     nil))))]
    (load-project app location)
    app))

(defmethod handle-request :new-project [event app]
  (if-let [project-data (fx/run<!! (dialogs/new-project (:stage app) (keys (:plugins app))
                                                        (fn [value]
                                                          (if-let [file (io/file value)]
                                                            (and (fs/absolute? file) (or (not (.exists file)) (and (.isDirectory file) (not (app/is-project-dir file)))))
                                                            ))))]
    (create-project app project-data)
    app))

(defmethod handle-request :stop-job [event app]
  (fx/run<!!
    (main-gui/add-app-log-entry! (:main-screen app) (str "Job: " (:name (:job app)) " stopped.")))
  (stop-current-job app))

(defmethod handle-request :job-finished [{:keys [type]} app]
  (fx/run<!!
    (main-gui/add-app-log-entry! (:main-screen app) (str "Job: " (:name (:job app)) " finished.")))
  (stop-current-job app))

(defmethod handle-request :log-datamining [{:keys [type message]} app]
  (fx/run<!!
    (main-gui/add-dm-log-entry! (:main-screen app) message))
  app)

(defmethod handle-request :log-app [{:keys [type message]} app]
  (fx/run<!!
    (main-gui/add-app-log-entry! (:main-screen app) message))
  app)

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
        (if (is-model-valid app)
          (do
            (app-fn app))
          (do
            (fx/run<!! (dialogs/error-alert (:stage app) "Input files contain errors" (str "Cannot " operation " because input files contain invalid data. Please check input menu for files marked red and correct them.")))
            (switch-page app (get-first-invalid app)))))
      app)))

(defmethod handle-request :build [event app]
  (do-with-saved-project app "build"
                         (fn [app]
                           (start-job app "Build" #(build-working-dir app)))))

(defmethod handle-request :run [event app]
  (do-with-saved-project app "run"
                         (fn [app]
                           (start-job app "Run" #(run-learning app))
                           )))

(defmethod handle-request :build-run [event app]
  (do-with-saved-project app "build and run"
                         (fn [app]
                           (start-job app "Build & Run"
                                      #(do
                                        (build-working-dir app)
                                        (run-learning app))))))

(defmethod handle-request :delete-file [{:keys [key]} app]
  (if-let [file (core/item-key-to-file (:project app) key)]
    (when (.exists file)
      (fs/delete-dir file)))
  app)

(defmethod handle-request :open-location [{:keys [key]} app]
  (if-let [file (core/item-key-to-file (:project app) key)]
    (when (.exists file)
      (let [file (if (fs/directory? file)
                   file
                   (fs/parent file))]
        (-> (Desktop/getDesktop)
            (.open file)))))
  app)

(defmethod handle-request :save-project [event app]
  (let [unsaved (unsaved-pages app)]
    (save-model-pages app unsaved)))

(defmethod handle-request :new-relation [{:keys [key]} app]
  (if-let [rel (fx/run<!! (dialogs/new-relation (:stage app) (app/plugin-parser-context app)))]
    (let [file (core/item-key-to-file (:project app) [:relations (core/relation-to-filename rel)])]
      (fs/create file)
      ))
  app)

(defmulti handle-fs-change (fn [[event file time] app]
                             event))

(defn load-model-page [app file]
  (let [file-key (core/file-to-item-key (:project app) file)
        app (if-not (get-in (:project app) (apply core/model-map-keys (butlast file-key)))
              ; parent dir not found - load
              (load-model-page app (fs/parent file))
              app
              )]
    (fx/run<!!
      (main-gui/add-menu-files! (:main-screen app) (list {:name (last file-key) :path file-key})))
    (-> app
        (app/load-file-to-model file)
        (handle-model-modified app file-key))))

(defn load-pages [app files]
  (reduce load-model-page app files))

(defmethod handle-fs-change :create [[event file time] app]
  (let [file-key (core/file-to-item-key (:project app) (fs/file file))]
    (if (get-in (:project app) (apply core/model-map-keys file-key))
      app
      (load-model-page app file))))

(defn- is-page-modified-or-current [app key]
  (or (= key (:current-page-key app))
      (is-page-modified app key)))

(defn mark-page-desynced [app page-key]
  (let [app (app/mark-file-desynced app page-key)]
    (fx/run<!!
      (update-main-menu-for-current-page! app)
      (update-file-menu-for-page! app page-key))
    app))

(defmethod handle-fs-change :modify [[event ^File file time] app]
  ; skip events on dirs
  (if (.isDirectory file)
    app
    (let [file-key (core/file-to-item-key (:project app) file)]
      (if-not (and (fs/exists? file) (get-in (:project app) (apply core/model-map-keys file-key)))
        app
        (let [{:keys [app needs-sync]} (app/update-fs-sync-status app file-key time)]
          (if-not needs-sync
            app
            (if (or (not (is-page-modified-or-current app (core/file-to-item-key (:project app) file)))
                    (fx/run<!! (dialogs/reload-modified-file (:stage app) file)))
              (reload-model-page app file-key)
              (mark-page-desynced app file-key))
            ))))))

(defn close-page-if-open [app key]
  (if (= key (:current-page-key app))
    (switch-page app [])
    app))

(defn remove-model-page [app file-key]
  (let [app (remove-pages app (app/get-model-item-keys-by-key (:project app) file-key false))]
    (fx/run<!!
      (main-gui/remove-file! (:main-screen app) file-key))
    (-> app
        (close-page-if-open file-key)
        (app/delete-model-page file-key)
        (dissoc-in [:pages file-key])
        (handle-model-modified app file-key))))

(defn remove-pages [app file-keys]
  (reduce remove-model-page app file-keys))

(defmethod handle-fs-change :delete [[event file time] app]
  (let [file-key (core/file-to-item-key (:project app) (fs/file file))]
    (if-not (get-in (:project app) (apply core/model-map-keys file-key))
      app
      (remove-model-page app file-key))))


(defn apply-diff-to-pages [app ^ModelDiff diff]
  (if (and diff (instance? ModelDiff diff))
    (let [app (reduce (fn [app [key item :as kitem]]
                        (let [app (-> app
                                      (assoc :project (core/set-model-item (:project app) kitem))
                                      (app/save-model-to-file key)
                                      (revert-model-page key)
                                      (handle-model-modified app key))]
                          (fs/delete-dir (core/item-key-to-file (:project app) key))
                          app
                          )
                        ) app (:set diff))
          app (reduce (fn [app key]
                        (let [app (remove-model-page app key)]
                          (fs/delete-dir (core/item-key-to-file (:project app) key))
                          app
                          )
                        ) app (:remove diff))]
      app)
    app))

(defn handle-exception [app e]
  (let [writer (StringWriter.)]
    (println "An application error occured:")
    (stacktrace/print-cause-trace e)
    (binding [*out* writer]
      (println "An application error occured:")
      (stacktrace/print-cause-trace e))
    (fx/run<!!
      (main-gui/add-app-log-entry! (:main-screen app) (str writer)))
    app))

(defn handle-window-closed [app]
  (-> app
      (switch-page [])
      (dissoc :pages)
      (stop-current-job))
  (when-let [c (get app :fs-changes)]
    (watch-fs/close! c))
  (swap! instances dec)
  (when (and (not (core/dev?)) (zero? @instances))
    (System/exit 0)))

(defn handle-close-request [app ^WindowEvent e]
  (let [unsaved (unsaved-pages app)
        save (if (empty? unsaved)
               :no
               (dialogs/save-modified-on-exit (:stage app)))]
    (case save
      :yes [(save-model-pages app unsaved) false]
      :no [app false]
      :cancel (do
                (.consume e)
                [app true]))))

(defn show-main-scene [stage]
  (let [app (init-app stage)
        app (if (core/dev?)
              (-> app
                  (load-project (fs/file "dev-resources/projects/aleph_default/"))
                  ;(load-project (fs/file "dev-resources/projects/ace_tilde/"))
                  )
              app)

        close (chan)
        app-ref (atom app)
        ]
    (swap! instances inc)
    (.setOnCloseRequest stage (reify EventHandler
                                (handle [this e]
                                  (put! close (handle-close-request @app-ref e)))))
    (go
      (handle-window-closed
        (loop [app app]
          (let [[next-app continue] (try
                                      (alt! (:ui-requests app) ([ui-request]
                                                                 [(handle-request ui-request app) true])
                                            (:fs-changes app) ([change]
                                                                [(handle-fs-change change app) true])
                                            close ([result]
                                                    result))
                                      (catch Exception e
                                        [(handle-exception app e) true]
                                        ))]
            (reset! app-ref next-app)
            (if continue
              (recur next-app)
              next-app)))))

    (.setScene stage (Scene. (main-gui/get-widget (:main-screen app))))
    (.show stage)))

(defmethod handle-request :new-window [event app]
  (fx/run<!! (show-main-scene (fx/stage)))
  app)

(when (core/dev?)
  (watch-fs/stop-all-channel-watchers!)
  (fx/run<!! (show-main-scene (fx/stage))))