(ns urdmi.watch-fs
  (:require [clojure.core.async :as async]
            [me.raynes.fs :as fs])
  (:import (java.nio.file WatchService Paths FileSystems StandardWatchEventKinds WatchEvent)
           (java.util Date)
           (javafx.scene.shape Path)))

(def ENTRY_CREATE java.nio.file.StandardWatchEventKinds/ENTRY_CREATE)
(def ENTRY_DELETE java.nio.file.StandardWatchEventKinds/ENTRY_DELETE)
(def ENTRY_MODIFY java.nio.file.StandardWatchEventKinds/ENTRY_MODIFY)

(defn register [{:keys [path event-types callback options] :as spec}
                watcher keys]
  (letfn [(register-helper
            [{:keys [path event-types callback options]} watcher keys]
            (let [; make-array is needed because Paths/get is a variadic method
                  ; Java compiler handles variadic method automatically, but when
                  ; using Clojure it's necessary to manually supply an array at
                  ; the end.
                  dir (Paths/get path (make-array String 0))
                  types (reduce (fn [acc type]
                                  (case type
                                    :create (conj acc ENTRY_CREATE)
                                    :delete (conj acc ENTRY_DELETE)
                                    :modify (conj acc ENTRY_MODIFY)))
                                []
                                event-types)

                  modifier (try
                             (let [c (Class/forName "com.sun.nio.file.SensitivityWatchEventModifier")
                                   f (.getField c "HIGH")]
                               (.get f c))
                             (catch Exception e))

                  modifiers (when modifier
                              (doto (make-array java.nio.file.WatchEvent$Modifier 1)
                                (aset 0 modifier)))

                  key (if modifiers
                        (.register dir watcher (into-array types) modifiers)
                        (.register dir watcher (into-array types)))]

              (assoc keys key [dir callback spec])))]
    (register-helper spec watcher keys)))

(defn- kind-to-key [kind]
  (case kind
    "ENTRY_CREATE" :create
    "ENTRY_MODIFY" :modify
    "ENTRY_DELETE" :delete))

(defn- add-path-event [paths-to-events [kind path time :as event] callback]
  (let [prev-events (get paths-to-events path {:callback callback
                                               :path     path
                                               :events   (list)})]
    (assoc paths-to-events path
                           (-> prev-events
                               (assoc :time time)
                               (update :events conj kind)))))

(defn- reduce-to-single-event [{:keys [path events time] :as path-events}]
  (let [priority-events (remove #{:modify} events)]
    (if (empty? priority-events)
      [:modify path time]
      [(cond (= :delete (first priority-events)) :delete ; delete last - reduce to delete
             (< 1 (count (set priority-events))) :modify ;delete/create reduce to modify
             true :create
         ) path time])))

(defn- register-created-dir [file watcher spec event-time keys paths-to-events callback]
  ; because register doesn't catch up immediately, add the already existing files and dirs
  (doseq [[base-dir subdir-names file-names] (fs/iterate-dir file)]
    (println file-names)
    (swap! keys #(register (assoc spec :path (str base-dir)) watcher %))
    (doseq [file-name (concat file-names subdir-names)]
      (swap! paths-to-events add-path-event [:create (fs/file base-dir file-name) event-time] callback))))

(defn- watch [^WatchService watcher keys paths-to-events]
  (if-let [key (.poll watcher)]
    (let [[dir callback spec] (@keys key)
          event-time (Date.)
          events (for [event (.pollEvents key)]
                   (let [op (kind-to-key (.. event kind name))
                         file (->> ^WatchEvent event
                                   .context
                                   (.resolve dir)
                                   (.toFile))]
                     [op file]))]
      (doseq [[op file :as event] events]
        (swap! paths-to-events add-path-event (conj event event-time) callback)
        ;add watch to newly created dirs
        (when (and (.isDirectory file)
                   (= op :create))
          (future (register-created-dir file watcher spec event-time keys paths-to-events callback))))
      (.reset key)
      (recur watcher keys paths-to-events))
    (do
      (doseq [[path path-events] (sort-by (fn [a]
                                            (-> a
                                                (second)
                                                (:time))) (seq @paths-to-events))]
        (when (> (- (System/currentTimeMillis) (.getTime ^Date (:time path-events)))
                 1000)
          (apply (:callback path-events) (reduce-to-single-event path-events))
          (swap! paths-to-events dissoc path)))
      (Thread/sleep 100)
      (recur watcher keys paths-to-events)
      )))

(defn start-watch [specs]
  (letfn [(handle-recursive
            [specs]
            (reduce
              (fn [acc
                   {:keys [path event-types callback options bootstrap] :as spec}]
                (if bootstrap
                  (bootstrap path))
                (if (:recursive options)
                  (let [f (clojure.java.io/file path)
                        fs (file-seq f)
                        acc (ref acc)]
                    (do
                      (doall
                        (pmap
                          (fn [file]
                            (if (.isDirectory file)
                              (dosync
                                (commute
                                  acc
                                  #(conj % (assoc spec :path (str file))))))) fs))
                      (deref acc)))
                  (conj acc spec)))
              []
              specs))]
    (let [specs (handle-recursive specs)
          watcher (.. FileSystems getDefault newWatchService)
          keys (atom (reduce (fn [keys spec]
                               (register spec watcher keys)) {} specs))
          close-watcher (fn []
                          (.close watcher))]
      (future (watch watcher keys (atom {})))
      close-watcher)))

(def ^:private closing-fns-for-channels (atom {}))

(defn stop-all-channel-watchers! []
  (doseq [[key closing-fn] @closing-fns-for-channels]
    (closing-fn)))

(defn- stop-channel-watcher!
  "Stops all watches for given channel"
  [c]
  (swap! closing-fns-for-channels
         (fn [closing-fns-for-channels]
           (if-let [close-fn (get closing-fns-for-channels c nil)]
             (close-fn))
           (dissoc closing-fns-for-channels c))
         ))

(defn close!
  "Closes given channel and immediately stops watching the dirs.
  With regular clojure.core.async/close the watcher will be stopped with a delay"
  [c]
  (stop-channel-watcher! c)
  (async/close! c)
  )

(defn changes-chan
  "Given a path or seq of paths as strings/file, sets up watchers in individual
threads for each path and then returns a channel that receives all filesystem
modifications within those paths."
  [paths]
  (let [paths (if (coll? paths) paths (vector paths))
        channel (async/chan)]
    (swap! closing-fns-for-channels conj
           [channel
            (start-watch
              (mapv (fn [path] {:path        path
                                :event-types [:create :modify :delete]
                                :bootstrap   nil
                                :callback    (fn [op file time] (async/put! channel [op file time]
                                                                            (fn [open]
                                                                              (when-not open
                                                                                (stop-channel-watcher! channel)))))
                                :options     {:recursive true}})
                    paths))])
    channel))

(stop-all-channel-watchers!)