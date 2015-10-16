(defproject com.codingdeficiency/urdmi "0.1.0-SNAPSHOT"
            :description "Universal Relational Data Mining Interface"
            :url "https://github.com/QAston/URDMI"
            :license {:name "GNU General Public License, version 3"
                      :url  "https://www.gnu.org/licenses/gpl.html"}
            :dependencies [[org.clojure/clojure "1.7.0"]
                           [clj-antlr "0.2.2"]
                           [environ "1.0.1"]
                           [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                           [org.clojars.qaston/fx-clj "0.2.1-SNAPSHOT" :exclusions [org.clojure/core.async]]
                           [differ "0.2.1"]
                           [org.clojure/core.incubator "0.1.3"]
                           [org.controlsfx/controlsfx "8.40.9"]
                           [com.stuartsierra/component "0.2.3"]
                           [org.fxmisc.richtext/richtextfx	"0.6.10"]
                           [org.apache.directory.studio/org.apache.commons.io "2.4"]
                           [org.apache.commons/commons-lang3 "3.4"]
                           [potemkin "0.4.1"]
                           [me.raynes/fs "1.4.6" :exclusions [potemkin]]
                           [org.clojars.ato/clojure-jsr223 "1.5.1" :exclusions [org.clojure/clojure]]
                           ]
            :profiles {:dev {:dependencies [[midje "1.7.0-beta1"]]
                             :plugins      [[codox "0.8.13"]
                                            [lein-midje "3.0.0"]
                                            [lein-environ "1.0.1"]]
                             :env {:urdmi-development "true"}}
                       :uberjar {:main urdmi.launcher
                             :aot [urdmi.gui-app]}}
            :plugins []
            :java-source-paths ["deps/jiprolog/src" "javasrc"]
            :javac-options ["-target" "1.8" "-source" "1.8"]
            ;:jvm-opts ["-Xmx2g"]
            :resource-paths ["deps/jiprolog/src" "resources"] ; for jiprolog engine
            )
