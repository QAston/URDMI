(ns urdmi.plugin.aleph.output-parser
  (:require [clojure.string :as str]
            [urdmi.core :as core]))

(defn strip-section-name[^String s]
  (.substring s (inc (.indexOf s "]"))))

(defn parse-theory [str-seq]
  (let [rule-seq (->> str-seq
                     (remove str/blank?)
                     (partition-by (fn [s]
                                     (when (.startsWith s "[Rule ")
                                       s)))
                     (partition 2)
                      (map (fn [[rule-head rule-text]]
                             (let [head-data (read-string (strip-section-name (first rule-head)))
                                   pos-covered (nth head-data 3 )
                                   neg-covered (nth head-data 7 )]
                               {:pos-covered pos-covered :neg-covered neg-covered :text (str/join core/nl rule-text )}
                               ))))]
    rule-seq)
  )

(defn parse-training-results[s]
  (let [[[pos-egz-true neg-egz-true pos-egz-false neg-egz-false]](read-string (strip-section-name s))]
    {:pos-egz-true pos-egz-true :neg-egz-true neg-egz-true :pos-egz-false pos-egz-false :neg-egz-false neg-egz-false}))

(defn- find-section-begin[lines, sname]
  (drop-while (fn [line]
                (not (.startsWith line (str "[" sname "]")))) lines))

(defn lex
  "Returns a map from section header to lines in the section.
  in-str - string from aleph output.
  sections - a list of [section-name section-content] with sections names to extract.
  section-content - (fn[section-to-end-str-seq] section-str-seq) function that trims the str-seq to only contain section content"
  [in-str, sections]
  (let [lines (str/split-lines in-str)]
    (into {}
          (map (fn [[section-name section-content]]
                 (let [section-begining (find-section-begin lines, section-name)]
                   [section-name (when-not (empty? section-begining) (section-content section-begining))]))
               sections
               ))
    ))

(defn separate-theory-section
  [str-seq]
  (take-while (fn [line]
                (or (not (.startsWith line "["))
                  (.startsWith line (str "[Rule ")))) (rest str-seq)))

(defn read-theory-page-info [in-str]
  (let [training-label "Training set summary"
        lexed (lex in-str [["theory" separate-theory-section]
                     [training-label first]])]
    (when (and (lexed "theory") (lexed training-label))
      {:theory           (parse-theory (lexed "theory"))
       :training-results (parse-training-results (lexed training-label))})))