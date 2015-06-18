(defproject com.codingdeficiency/urdmi "0.1.0-SNAPSHOT"
  :description "Universal Relational Data Mining Interface"
  :url "https://github.com/QAston/URDMI"
  :license {:name "GNU General Public License, version 3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [com.igormaznitsa/prologparser "1.3.2"]
                 [clj-antlr "0.2.2"]
                 [fx-clj "0.2.0-alpha1"]
                 [com.stuartsierra/component "0.2.3"]
                 ]
  :profiles {:dev {:dependencies [[midje "1.7.0-beta1"]]}}
  :plugins [[lein-midje "3.0.0"]])
