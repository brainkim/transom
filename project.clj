(defproject transom "0.1.0-SNAPSHOT"
  :description "Operational Transformations in Clojure/Clojurescript"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2644"]]
  :source-paths ["src/clj" "src/cljx"]
  :test-paths ["test" "target/test-classes"]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.5.8"]
                             [om "0.6.4"]
                             [ring/ring "1.2.1"]
                             [compojure "1.1.6"]
                             [fogus/ring-edn "0.2.0"]
                             [http-kit "2.1.16"]
                             [com.stuartsierra/component "0.2.1"]
                             [org.clojure/core.async "0.1.278.0-76b25b-alpha"]]
              :plugins [[lein-cljsbuild "1.0.4"]
                        [com.keminglabs/cljx "0.5.0"]]
              :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
              :source-paths ["examples/clj"]
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
