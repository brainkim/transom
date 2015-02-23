(ns transom.examples.core
  (:require [transom.examples.server :as server]
            [com.stuartsierra.component :as component]))

(def !system (atom nil))

(defn start
  []
  (let [system (component/system-map
                 :server
                 (server/server 8000))]
    (reset! !system system)))

(defn stop
  []
  (component/stop @!system))

(defn -main
  []
  (start)
  (component/start @!system))
