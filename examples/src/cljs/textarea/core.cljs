(ns textarea.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan >! <! alts!]]
            [transom.core :as transom]))

(enable-console-print!)

(def websocket* (atom nil))
(def text-chan* (atom (chan)))
(def edit-chan* (atom (chan)))
(def buffer*    (atom (vector)))

(defn send-message [message]
  (if (empty? @buffer*)
    (.send @websocket* (pr-str {:message message})) 
    (swap! buffer* conj message)))

(defn receive-message [message]
  (println message)
  (when-let [pending-message (first @buffer*)]
    (send-message pending-message)
    (swap! buffer* rest)))

(defn create-socket
  []
  (println "establishing websocket...")
  (reset! websocket* (js/WebSocket. "ws://localhost:8000/ws"))
  (aset @websocket* "onopen"    #(.send @websocket* (pr-str {:hello "world"})))
  (aset @websocket* "onclose"   #(println "close: chan"))
  (aset @websocket* "onerror"   #(println "error: " %))
  (aset @websocket* "onmessage" #(receive-message (.-data %))))

(defn destroy-socket
  []
  (.close @websocket*)
  (reset! @websocket* nil))

;; Rendering bullshit
(defn change
  [e cursor]
  (let [message (.. e -target -value)]
    (put! @text-chan* message)
    (om/update! cursor [:current] message)))

(defn collab-textarea
  [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:kill-chan (chan)})
    om/IDidMount
    (did-mount [_]
      (create-socket)
      (go-loop [prev ""]
        (let [kill-chan (om/get-state owner :kill-chan)
              text-chan @text-chan*
              [text c] (alts! [text-chan kill-chan])]
          (when-not (= kill-chan c)
            (>! @edit-chan* (transom/pack (transom/diff prev text)))
            (recur text))))
      (go-loop []
        (let [kill-chan (om/get-state owner :kill-chan)
              edit-chan @edit-chan*
              [edit c] (alts! [edit-chan kill-chan])]
          (when-not (= kill-chan c)
            (println edit)
            (swap! buffer* transom/compose edit)
            (recur)))))
    om/IWillUnmount
    (will-unmount [_]
      (let [kill-chan (om/get-state owner :kill-chan)]
        (put! kill-chan (.now js/Date))))
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
