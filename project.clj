(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [clojure-future-spec "1.9.0-beta4"]
                 [org.clojure/test.check "0.10.0-alpha2"]]
  :main ^:skip-aot advent-of-code.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
