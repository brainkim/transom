(defproject transom "0.1.0-SNAPSHOT"
  :description "FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.keminglabs/cljx "0.3.2"] 
                 [org.clojure/core.match "0.2.1"]]
  :plugins [[com.keminglabs/cljx "0.3.2"]]
  :hooks [cljx.hooks]
  :profiles {:dev {:dependencies [[midje "1.4.0"]]
                   :plugins [[lein-midje "2.0.1"]]
                   :cljx {:builds [{:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :clj}
                                   {:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :cljs}]}}})
