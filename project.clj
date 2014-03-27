(defproject transom "0.1.0-SNAPSHOT"
  :description "FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [com.keminglabs/cljx "0.3.2"]
                 [org.clojure/core.match "0.2.1"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [com.keminglabs/cljx "0.3.2"]]
  :hooks [cljx.hooks]
  :profiles {:dev
             {:dependencies [[midje "1.4.0"]
                             [om "0.5.3"]
                             [org.clojure/core.async "0.1.278.0-76b25b-alpha"] 
                             [ring/ring "1.2.1"]
                             [compojure "1.1.6"]
                             [fogus/ring-edn "0.2.0"]
                             [http-kit "2.1.16"]]
              :plugins [[lein-midje "2.0.1"]]

              ;clj
              :source-paths ["examples/src/clj"]
              :main textarea.core

              :cljx
              {:builds [{:source-paths ["src/cljx"]
                         :output-path "target/classes"
                         :rules :clj}
                        {:source-paths ["src/cljx"]
                         :output-path "target/classes"
                         :rules :cljs}]}

              :cljsbuild
              {:builds [{:id "textarea"
                         :source-paths ["target/classes"
                                        "examples/src/cljs"]
                         :compiler {:output-to "resources/main.js"
                                    :output-dir  "resources/out"
                                    :source-map true
                                    :optimizations :none}}]}

              :jvm-opts ^:replace ["-Xmx1g" "-server"]}})
