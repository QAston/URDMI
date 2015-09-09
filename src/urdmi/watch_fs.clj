(ns urdmi.watch-fs
  (:require [clojure.core.async :as async])
  (:import (java.nio.file WatchService Paths FileSystems StandardWatchEventKinds)))

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

                  modifier  (try
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

(defn- remove-redundant-events [events]
  (let [events-by-file (->> events
                            (sort-by second)
                            (partition-by second))]
    (mapcat (fn [file-events]
              (let [file-by-op (into {} file-events)]
                (if (>= 1 (count file-by-op))
                  (seq file-by-op)
                  (seq (dissoc file-by-op :modify)))))
            events-by-file)))

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
                          (register spec watcher keys)) {} specs))]
      (letfn [(kind-to-key [kind]
                           (case kind
                             "ENTRY_CREATE" :create
                             "ENTRY_MODIFY" :modify
                             "ENTRY_DELETE" :delete))
              (watch [watcher keys]
                     (let [key (.take watcher)
                           [dir callback spec] (@keys key)
                           event-time (java.util.Date.)
                           events (remove-redundant-events
                                    (for [event (.pollEvents key)]
                                      (let [kind (kind-to-key (.. event kind name))
                                            name (->> event
                                                      .context
                                                      (.resolve dir)
                                                      (.toFile))]
                                        [kind name])))]
                       (doseq [[op file :as event] events]
                         (when (and (.isDirectory file)
                                    (= op :create))
                           (swap! keys #(register (assoc spec :path (str file)) watcher %))
                           )
                         (apply callback (conj event event-time)))
                       (.reset key)
                       (recur watcher keys)))
              (close-watcher []
                             (.close watcher))]
        (future (watch watcher keys))
        close-watcher))))

(def ^:private closing-fns-for-channels (atom {}))

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