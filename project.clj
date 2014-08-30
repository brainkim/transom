(defproject transom "0.1.0-SNAPSHOT"
  :description "Operational Transformations in Clojure/Clojurescript"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2268"]
                 [com.keminglabs/cljx "0.3.2"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [org.clojure/core.typed "0.2.48"]]
  :source-paths ["src/clj" "src/cljx"]
  :test-paths ["target/test-classes"]
  :profiles {:dev
             {:dependencies [[om "0.6.4"]
                             [ring/ring "1.2.1"]
                             [compojure "1.1.6"]
                             [fogus/ring-edn "0.2.0"]
                             [http-kit "2.1.16"]
                             [com.stuartsierra/component "0.2.1"]
                             [org.clojure/test.check "0.5.7"]
                             [com.cemerick/clojurescript.test "0.3.0"]
                             [com.cemerick/double-check "0.5.7"]
                             [bidi "1.10.2"]]
              :plugins [[lein-cljsbuild "1.0.3"]
                        [com.keminglabs/cljx "0.4.0"]]
              ;clj
              :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
              :source-paths ["examples/clj"]
              :jvm-opts ^:replace ["-server" "-Xms3072m" "-Xmx3072m"]
              :cljsbuild
              {:builds [{:id "textarea"
                         :source-paths ["target/classes"
                                        "examples/cljs"]
                         :compiler {:output-to "resources/main.js"
                                    :output-dir "resources/out"
                                    :source-map true
                                    :libs [""] ;; grumble grumble
                                    :optimizations :none}}]}}}
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}
                  {:source-paths ["test"]
                   :output-path "target/test-classes"
                   :rules :clj}]})
