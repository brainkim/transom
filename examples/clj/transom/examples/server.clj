(ns transom.examples.server
  (:require [clojure.core.async :as async :refer [go go-loop <! >! put! chan alt! close!]]
            [ring.util.response :refer [file-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.edn :refer [wrap-edn-params]]
            [org.httpkit.server :refer [run-server with-channel on-close on-receive send!]]
            [compojure.core :refer [routes GET]]
            [compojure.route :as route]
            [transom.core :as transom]
            [com.stuartsierra.component :as component])
  (:gen-class))

(defonce !server (atom nil))
(defonce !channels (atom #{}))
(def killer (chan))
(def inbox (chan))

(defn web-socket [request]
  (with-channel request c
    (on-close c (fn [status] (swap! !channels disj c)))
    (on-receive c (fn [data] (put! inbox [(read-string data) c])))))

(defn app
  []
  (-> (routes (GET "/textarea" [] web-socket)
              (route/files "/" {:root "resources"})
              (route/not-found "¯\\_(ツ)_/¯"))
      wrap-edn-params))

(defn send-all!
  [message]
  (let [message (pr-str message)]
    (doseq [c @channels!]
      (send! c message))))

(defn stop-server
  []
  (when-not (nil? @server!)
    (@server! :timeout 100)
    (reset! server! nil)
    (reset! channels! #{})
    (put! killer :kill)
    (println "Server stoppped!")))

(defn start-server
  []
  (let [ids! (async/to-chan (range))
        document! (atom {:doc "" :version 0})
        inbox* (async/pub inbox #(get-in % [0 :type]))
        inits  (async/sub inbox* :init (chan))
        edits  (async/sub inbox* :edit (chan))]
    (go-loop []
      (alt!
        killer
        ([_]
          (println "You killed me"))
        inits
        ([[data sock]]
          (swap! channels! conj sock)
          (let [{:keys [version doc]} @document!
                response {:type :init
                          :id (<! ids!)
                          :version version
                          :doc doc}]
          (send! sock (pr-str response)))
          (recur))
        edits
        ([[data sock]]
          (swap! document!
                 (fn [{:keys [doc version] :as document}]
                   (if-let [{:keys [edit]} data]
                     {:doc (transom/patch doc edit)
                      :version (inc version)}
                     document)))
          (send-all! data)
          (recur))))
    (reset! server! (run-server (app) {:port 8000}))))

(defn -main
  []
  (start-server))
