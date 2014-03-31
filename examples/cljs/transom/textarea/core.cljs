(ns transom.textarea.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! chan >! <! alts!]]
            [cljs.reader :refer [read-string]]
            [transom.core :as transom]))

(enable-console-print!)

(defn collab-textarea
  [data owner]
  (letfn [(change [e cursor owner]
            (let [message (.. e -target -value)]
              (om/update! cursor [:doc] message)))]
    (reify
      om/IRender
      (render [_]
        (dom/textarea #js {:value (:doc data)
                           :autoFocus true
                           :rows 50
                           :cols 80
                           :onChange #(change % data owner)})))))

(defn diff
  [a b]
  (if (= a b)
    [:= (count a)]
    (letfn [(common-prefix [^String a ^String b]
              (loop [i 0]
                (if (= (.charAt a i) (.charAt b i))
                  (recur (inc i))
                  i)))
            (common-suffix [^String a ^String b]
              (let [m (count a)
                    n (count b)]
                (loop [i 0]
                  (if (= (.charAt a (- m i 1)) (.charAt b (- n i 1)))
                    (recur (inc i))
                    i))))]
      (let [p (common-prefix a b)
            s (common-suffix a b)]
        (transom/pack [[:= p]
                       [:- (- (count a) p s)]
                       [:+ (.slice b p (- (count b) s))]
                       [:= s]])))))

(defn send-edit
  [ws edit]
  (let [out {:edit edit, :id 0}]
    (.send ws (pr-str out))))

(defn patch-doc!
  [state! edit]
  (swap! state!
         (fn [state]
           (let [doc (:doc state)
                 doc (transom/patch doc edit)]
             (assoc state :doc doc)))))

(defn -main
  []
  (let [app-state! (atom {:doc ""})
        edits (chan)
        inbox (chan)
        websocket (js/WebSocket. "ws://localhost:8000/ws")] 
    (aset websocket "onopen" #(.send websocket (pr-str {:hello "world"})))
    (aset websocket "onclose" #(println "close"))
    (aset websocket "onerror" #(println "error"))
    (aset websocket "onmessage" #(go (>! inbox %)))
    (go-loop [pending nil, buffer nil]
      (alt!
        edits ([edit]
               (if (nil? pending)
                 (do
                   (send-edit websocket edit)
                   (recur edit buffer))
                 (if (nil? buffer)
                   (recur pending edit)
                   (recur pending (transom/compose buffer edit)))))
        inbox ([message]
               (let [data (read-string (.-data message))
                     {:keys [id edit]} data]
                 (println data)
                 (when edit
                   (if (= id 0)
                     (if-let [buffer buffer]
                       (do
                         (send-edit websocket buffer)
                         (recur buffer nil))
                       (recur nil []))
                     (if (nil? pending)
                       (patch-doc! app-state! edit)
                       (let [[pending' edit'] (transom/transform pending edit)]
                         (if (nil? buffer)
                           (do
                             (patch-doc! app-state! edit')
                             (recur pending' buffer))
                           (let
                             [[buffer' edit''] (transom/transform buffer edit')]
                             (patch-doc! app-state! edit'')
                             (recur pending' buffer')))))))))))
    (om/root
      collab-textarea
      app-state! 
      {:target (.getElementById js/document "app")
       :tx-listen (fn [{:keys [old-value new-value] :as tx} _]
                    (put! edits (transom/pack (diff old-value new-value))))})))

(-main)
