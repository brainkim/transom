(ns transom.examples.server
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan put! alt! close!]]
            [clojure.tools.reader.edn :as edn]
            [ring.util.response :refer [file-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.edn :refer [wrap-edn-params]]
            [org.httpkit.server :as httpkit :refer [with-channel on-close on-receive send! run-server]]
            [compojure.core :refer [routes GET]]
            [compojure.route :as route]
            [transom.core :as transom]
            [transom.document :as document]))

(defonce !server (atom nil))
(defonce !channels (atom #{}))
(def killer (chan))
(def inbox (chan))

(defn websocket [request]
  (with-channel request c
    (on-close c (fn [_] (swap! !channels disj c)))
    (on-receive c (fn [data]
                    (swap! !channels conj c)
                    (put! inbox (edn/read-string data))))))

(defn app
  []
  (-> (routes (GET "/textarea" [] websocket)
              (route/files "/" {:root "resources"})
              (route/not-found "¯\\_(ツ)_/¯"))
      wrap-edn-params))

(defn send-all!
  [message]
  (doseq [c @!channels]
    (send! c message)))

(defn stop-server
  []
  (when-not (nil? @!server)
    (@!server :timeout 100)
    (put! killer :kill)
    (reset! !server nil)
    (reset! !channels #{})
    (println "Server stoppped!")))

(defn start-server
  []
  (let [!ids (async/to-chan (range))
        !doc (atom (document/document))
        inbox-type (async/pub inbox :type)
        inits (async/sub inbox-type :init (chan))
        edits (async/sub inbox-type :edit (chan))]
    (go-loop []
      (alt!
        (timeout 500)
        ([_]
          (???)
          (recur))
        killer
        ([_]
          (println "you killed me too!"))))
    (go-loop []
      (alt!
        edits
        ([{:keys [edit version id] :as message}]
          (println edit version id)
          (swap! !doc
            (fn [doc]
              (let [edit (document/transform-edit doc edit version)
                    doc (document/patch doc edit)
                    version (document/version doc)])))
          (recur))
        killer
        ([_]
          (println "you killed me."))))
    (reset! !server (run-server (app) {:port 8000}))
    (println "Server started!")))

(defn -main
  []
  (start-server))
