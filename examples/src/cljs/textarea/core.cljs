(ns textarea.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan alts!]]
            [transom.core :as transom]))

(enable-console-print!)

;; Websocket bullshit
(def websocket* (atom nil))

(defn- send-message [c] (.send @websocket* (pr-str {:message c})))

(defn create-socket
  []
  (println "establishing websocket...")
  (reset! websocket* (js/WebSocket. "ws://localhost:8000/ws"))
  (aset @websocket* "onopen"    #(.send @websocket* (pr-str {:hello "world"})))
  (aset @websocket* "onclose"   #(println "close: chan"))
  (aset @websocket* "onerror"   #(println "error: " %))
  (aset @websocket* "onmessage" #(let [data (.-data %)]
                                   (println data))))

(defn destroy-socket
  []
  (.close @websocket*)
  (reset! @websocket* nil))

;; Rendering bullshit
(defn change
  [e cursor]
  (send-message (diff/diff "" (.. e -target -value)))
  (om/update! cursor [:current] (.. e -target -value)))

(defn collab-textarea
  [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/textarea #js {:value (:current data)
                         :autoFocus true 
                         :rows 50
                         :cols 80
                         :onChange #(change % data)}))))

(om/root
  collab-textarea
  {:current ""}
  {:target (.getElementById js/document "app")})

(create-socket)
