(ns transom.examples.core
  (:require [transom.examples.server :as server]
            [transom.examples.rooms :as rooms]
            [com.stuartsierra.component :as component]))

(def !system (atom nil))

(defn start
  []
  (let [system (component/system-map
                 :rooms (rooms/rooms)
                 :server
                 (component/using
                   (server/server 8000)
                   [:rooms]))]
    (reset! !system system)))

(defn stop
  []
  (component/stop @!system))

(defn -main
  []
  (start)
  (component/start @!system))
