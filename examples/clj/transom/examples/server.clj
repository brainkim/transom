(ns transom.examples.server
  (:require [clojure.core.async :as async]
            [clojure.tools.reader.edn :as edn]
            [ring.middleware.edn :refer [wrap-edn-params]]
            [ring.middleware.session :refer [wrap-session]]
            [org.httpkit.server :as server]
            [compojure.core :as compojure :refer [defroutes GET]]
            [compojure.route :as route]
            [com.stuartsierra.component :as component]))

(defn websocket [req]
  (let [example (get-in req [:route-params :example])
        room (get-in req [:rooms (keyword example)])]
    (server/with-channel req chan
      (server/on-close chan
        (fn [_]
          (async/put! room [{:type :close} chan])))
      (server/on-receive chan
        (fn [data]
          (async/put! room [(edn/read-string data) chan]))))))

(defn wrap-rooms
  [app rooms]
  (fn [req]
    (let [req (assoc req :rooms rooms)]
      (app req))))

(defroutes routes
  (GET "/ws/:example" [example] websocket)
  (route/files "/" {:root "resources"})
  (route/not-found "404 ¯\\_(ツ)_/¯"))

(defrecord Server
  [port rooms container]
  component/Lifecycle
  (start [this]
    (let [app (-> routes
                  wrap-edn-params
                  (wrap-rooms rooms))]
      (println "Server started!")
      (assoc this :container (server/run-server app {:port port}))))
  (stop [this]
    (println "Server stopped!")  
    (container :timeout 100)
    this))

(defn server
  [port]
  (map->Server
    {:port port}))
