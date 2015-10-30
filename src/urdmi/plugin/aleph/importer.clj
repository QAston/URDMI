(ns urdmi.plugin.aleph.importer
  (:require [urdmi.prolog :as prolog]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(defn- functor-filter [{:keys [type]}]
  (= type :ast-functor))

(defn- relation-sig [{:keys [children]}]
  [(:name (first children)) (dec (count children))]
  )

(defn- import-b [file dest-dir]
  (let [parser-context (prolog/aleph-parser-context)
        relation-rows (->> (prolog/prolog-sentence-seq parser-context (io/reader file))
                           (filter functor-filter)
                           (remove (fn [{:keys [type children]}]
                                     (= (:name (first children)) ":-")))
                           (doall))
        bg-settings-file (fs/file dest-dir "prolog-ext" "bg_and_settings.pl")

        lines-to-remove (set (map (fn [sen]
                                    (dec (:line (meta sen)))) relation-rows))

        bg-file-contents (keep-indexed (fn [index item]
                                         (if (lines-to-remove index)
                                           nil
                                           item)) (line-seq (io/reader (io/file file))))

        ]
    (fs/mkdirs (io/file dest-dir "prolog-ext"))
    (fs/mkdirs (io/file dest-dir "relations"))
    (with-open [writer (io/writer bg-settings-file)]
      (binding [*out* writer]
        (doseq [line bg-file-contents]
          (println line))))
    (doseq [[[name arity] asts] (group-by relation-sig relation-rows)]
      (with-open [writer (io/writer (io/file dest-dir "relations" (str name "_" arity ".pl")))]
        (prolog/pretty-print-sentences parser-context asts writer)))))

(defn- import-examples [file dest-dir, val-to-add]
  (let [parser-context (prolog/aleph-parser-context)
        relations-by-sig (->> (prolog/prolog-sentence-seq parser-context (io/reader file))
                              (filter functor-filter)
                              (map (fn [{:keys [children] :as m}]
                                     (assoc m :children (conj (vec children) {:type :ast-expression :value val-to-add}))))
                              (group-by relation-sig))
        ]
    (fs/mkdirs (io/file dest-dir "relations"))
    (doseq [[[name arity] asts] relations-by-sig]
      (with-open [writer (io/writer (io/file dest-dir "relations" (str name "_" arity ".pl")) :append true)]
        (prolog/pretty-print-sentences parser-context asts writer)))))

(defn import-project [src-dir dest-dir]
  (doseq [file (fs/glob src-dir "*.b")]
    (import-b file dest-dir))
  (doseq [file (fs/glob src-dir "*.n")]
    (import-examples file dest-dir 0))
  (doseq [file (fs/glob src-dir "*.f")]
    (import-examples file dest-dir 1)))