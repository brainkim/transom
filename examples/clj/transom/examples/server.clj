(ns transom.examples.server
  (:require [clojure.core.async :as async :refer [go go-loop <! >! put! chan alt! close!]]
            [ring.util.response :refer [file-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.edn :refer [wrap-edn-params]]
            [org.httpkit.server :refer [run-server with-channel on-close on-receive send!]]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [transom.core :as transom]))

(defonce server! (atom nil))
(defonce channels! (atom #{}))
(defonce inbox (chan))

(defn web-socket [request]
  (with-channel request c
    (on-close c (fn [status] (swap! channels! disj c)))
    (on-receive c (fn [data] (put! inbox [(read-string data) c])))))

(defroutes router
  (GET "/textarea" [] web-socket)
  (route/files "/" {:root "resources"})
  (route/not-found "¯\\_(ツ)_/¯"))

(defn app
  []
  (-> router
      wrap-edn-params))

(defn send-all!
  [message]
  (let [message (pr-str message)]
    (doseq [c @channels!]
      (send! c message))))

(defn stop-server
  [kill-chan]
  (when-not (nil? @server!)
    (@server! :timeout 100)
    (reset! server! nil)
    (reset! channels! #{})
    (put! kill-chan :kill)
    (println "Server stoppped!")))

(defn start-server
  []
  (let [kill (chan)
        ids! (async/to-chan (range))
        version! (async/to-chan (range))
        document! (atom "yr a big boob") 
        inbox* (async/pub inbox #(get-in % [0 :type]))
        inits (async/sub inbox* :init (chan))
        edits (async/sub inbox* :edit (chan))]
    (go-loop []
      (alt!
        kill ([_] println "POOPIE!")
        inits ([[data noob]]
                (println data)
                (swap! channels! conj noob)
                (let [response {:type :init
                                :id (<! ids!)
                                :doc @document!}]
                  (send! noob (pr-str response)))
                  (recur))
        edits ([[data chan]]
                (println data)
                (send-all! data)
                (recur))))
    (reset! server! (run-server (app) {:port 8000}))
    #(stop-server kill)))

(defn -main
  []
  (start-server))
