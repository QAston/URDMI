(ns urdmi.watch-fs-test
  (:use clojure.test)
  (:require [urdmi.watch-fs :as watch-fs]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.core.async :as async]))

(let [base-dir (fs/file "dev-resources/fs-tests")
      subdir (fs/file base-dir "testdir")
      testfile (fs/file base-dir "test.a")
      subdirtestfile (fs/file subdir "test.b")
      ]
  (async/go
    (try
      (fs/mkdir base-dir)
      (println "closing a channel")
      (let [c (watch-fs/changes-chan base-dir)]
        (watch-fs/close! c)
        (fs/create testfile)
        (fs/delete testfile)
        (is (= (async/<! c)
               nil)))


      (println "creating a file gives a single create event")
      (let [c (watch-fs/changes-chan base-dir)]
        ;(spit testfile "Tyryry")
        (fs/create testfile)
        (is (= (butlast (async/<! c)) [:create testfile]))
        (watch-fs/close! c)
        (is (= (async/<! c) nil)))
      (println "modifying file gives a single modify event")
      (let [c (watch-fs/changes-chan base-dir)]
        (spit testfile "Test content")
        (is (= (butlast (async/<! c)) [:modify testfile]))
        (watch-fs/close! c)
        (is (= (async/<! c) nil)))
      (println "deleting a file gives a delete event")
      (let [c (watch-fs/changes-chan base-dir)]
        (fs/delete testfile)
        (let [ev (butlast (async/<! c))
              ; skip modify event if present
              ev (if-not (= (first ev) :modify)
                   ev
                   (butlast (async/<! c)))]
          (is (= ev) [:delete testfile]))
        (watch-fs/close! c)
        (is (= (async/<! c) nil)))

      (let [c (watch-fs/changes-chan base-dir)]
        (fs/mkdir subdir)
        (println "creating a dir gives a single create event")
        (is (= (butlast (async/<! c)) [:create subdir]))
        (println "creating a file in a newly created dir gives a single create event")
        (fs/create subdirtestfile)
        (is (= (butlast (async/<! c)) [:create subdirtestfile]))
        (watch-fs/close! c)
        (is (= (async/<! c) nil)))
      (finally
        (fs/delete-dir base-dir)))))

