(ns textarea.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan >! <! alts!]]
            [transom.core :as transom]))

(enable-console-print!)


(def edit-chan* (atom (chan)))
(def buffer* (atom []))

(defn create-socket
  [cursor owner]
  (println "establishing websocket...")
  (let [websocket (js/WebSocket. "ws://localhost:8000/ws")]
    (aset websocket "onopen"    #(.send websocket (pr-str {:hello "world"})))
    (aset websocket "onclose"   #(println "close: chan"))
    (aset websocket "onerror"   #(println "error: " %))
    (aset websocket "onmessage" (fn [message]
                                  (let [data (.-data message)]
                                    )))
    (om/set-state! owner [:websocket] websocket)))

;; Rendering bullshit
(defn change
  [e cursor owner]
  (let [message (.. e -target -value)]
    (om/update! cursor [:text] message)))

(defn collab-textarea
  [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/textarea #js {:value (:current data)
                         :autoFocus true
                         :rows 50
                         :cols 80
                         :onChange #(change % data owner)}))))

(defn app
  [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:render? true})
    om/IRenderState
    (render-state [_ state]
      (dom/div nil
        (when (:render? state)
          (om/build collab-textarea data))
        (dom/button #js {:onClick
                         (fn [] (om/update-state! owner [:render?] #(not %)))}
                    "Toggle textarea")))))

(om/root
  app
  {:text ""}
  {:target (.getElementById js/document "app")
   :tx-listen (fn [{:keys [old-value new-value] :as pizza} _]
                (println (count old-value) (count new-value))
                (put! @edit-chan* (transom/pack (transom/diff old-value new-value))))})

(go-loop []
  (let [edit (<! @edit-chan*)]
    (println edit))
  (recur))
