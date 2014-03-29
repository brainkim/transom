(ns textarea.core
  (:require [ring.util.response :refer [file-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.edn :refer [wrap-edn-params]]
            [org.httpkit.server :refer [run-server with-channel on-close on-receive send!]]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [transom.core :as transom])
  (:import java.lang.Runtime))

(defonce server*   (atom nil))
(defonce channels* (atom #{}))
(defonce document* (atom ""))

(defn web-socket [request]
  (with-channel request channel
    (on-close channel   (fn [status]
                          (swap! channels* disj channel)
                          (println "channel closed")
                          (println @channels*)))
    (on-receive channel (fn [data]
                          (println data)
                          (swap! channels* conj channel)
                          (doseq [c @channels*]
                            (send! c data))))))

(defroutes router
  (GET "/ws" [] web-socket)
  (route/files "/" {:root "resources"})
  (route/not-found "¯\\_(ツ)_/¯"))

(def app
  (-> router
      wrap-edn-params))

(defn stop-server
  []
  (when-not (nil? @server*)
    (@server* :timeout 100)
    (reset! server* nil)
    (println "Server stoppped!")))

(defn start-server
  []
  (reset! server* (run-server #'app {:port 8000}))
  (println "Server started!")
  #(stop-server))

(defn send-message
  [message]
  (let [message (pr-str message)]
    (doseq [c @channels*]
      (send! c message))))

(defn -main
  []
  (.addShutdownHook (Runtime/getRuntime) (Thread. (start-server))))
