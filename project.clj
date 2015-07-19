(defproject com.codingdeficiency/urdmi "0.1.0-SNAPSHOT"
  :description "Universal Relational Data Mining Interface"
  :url "https://github.com/QAston/URDMI"
  :license {:name "GNU General Public License, version 3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-antlr "0.2.2"]
                 [fx-clj "0.2.0-alpha1"]
                 [com.stuartsierra/component "0.2.3"]
                 [org.apache.directory.studio/org.apache.commons.io "2.4"]
                 [potemkin "0.3.13"]
                 [me.raynes/fs "1.4.6" :exclusions [potemkin]]
                 [org.clojars.ato/clojure-jsr223 "1.5.1" :exclusions [org.clojure/clojure]]
                 ]
  :profiles {:dev {:dependencies [[midje "1.7.0-beta1"]]}}
  :plugins [[lein-midje "3.0.0"]]
  :java-source-paths ["deps/jiprolog/src"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :jvm-opts ["-Xmx2g"]
  :resource-paths ["deps/jiprolog/src" "resources"] ; for jiprolog engine

  )
