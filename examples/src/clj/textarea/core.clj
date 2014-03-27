(ns textarea.core
  (:require [ring.util.response :refer [file-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.edn :refer [wrap-edn-params]]
            [org.httpkit.server :refer :all]
            [org.httpkit.timer]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [transom.core :as transom]))

(defonce server (atom nil))

(def channels (atom #{}))

(defn sock [request]
  (with-channel request channel
    (on-close channel   (fn [status]
                          (swap! channels disj channel)
                          (println "channel closed")
                          (println @channels)))
    (on-receive channel (fn [data]
                          (println data)
                          (swap! channels conj channel)
                          (doseq [c @channels]
                            (send! c data))))))

(defroutes router
  (GET "/ws" [] sock)
  (route/files "/" {:root "resources"})
  (route/not-found "¯\\_(ツ)_/¯"))

(def app
  (-> router
      wrap-edn-params))

(defn stop-server
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main
  []
  (reset! server (run-server #'app {:port 8000}))
  (println "Server started!"))
