(ns transom.examples.textarea
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [cljs.core.async :as async :refer [put! chan >! <! alts!]]
            [cljs.reader :refer [read-string]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [transom.core :as transom]
            [goog.dom :as gdom]
            [goog.events :as events]
            [goog.net.WebSocket.EventType :refer [OPENED CLOSED ERROR MESSAGE]])
  (:import goog.net.WebSocket))
(enable-console-print!)

(defn host
  []
  (.. js/window -location -host)) 

(defn collaborative-textarea
  [data owner]
  (letfn [(change [e]
            (let [message (.. e -target -value)]
              (om/update! data [:doc] message)))]
    (reify
      om/IRender
      (render [_]
        (dom/textarea #js {:disabled (boolean (:doc data))
                           :value (or (:doc data) "")
                           :autoFocus true
                           :rows 50
                           :cols 80
                           :onChange change})))))

(defn app
  [data owner]
  (letfn [(change [e]
            (let [message (.. e -target -value)]
              (om/update! data [:doc] message)))]
    (reify
      om/IRender
      (render [_]
        (println "data "data)
        (println "should be nil" (:doc data))
        (println "should be false"(boolean nil))
        (println "should also be false" (boolean (:doc data)))
        (println "should be true" (not (:doc data)))
        (dom/div nil
          (dom/textarea #js {:disabled (not (:doc data))
                             :value (or (:doc data) "")
                             :autoFocus true
                             :rows 50
                             :cols 80
                             :onChange change}
          (dom/div nil (str "Data: "(pr-str data)))))))))

(defn diff
  [a b]
  (let [a-len (count a)
        b-len (count b)]
    (letfn
      [(common-prefix [a b]
         (loop [i 0]
           (if (and (< i a-len) (< i b-len)
                    (= (.charAt a i) (.charAt b i)))
             (recur (inc i))
             i)))
       (common-suffix [a b]
         (loop [i 0]
           (if (and (< i a-len) (< i b-len)
                    (= (.charAt a (- a-len i 1)) (.charAt b (- b-len i 1))))
             (recur (inc i))
             i)))]
      (if (= a b)
        [:= a-len]
        (let [pre (common-prefix a b)
              a' (subs a pre)
              suf (common-suffix a' b)]
          (transom/pack [[:= pre]
                         [:- (- a-len pre suf)]
                         [:+ (subs b pre (- b-len suf))]
                         [:= suf]]))))))

(defn conductor
  [in in* out out*]
  (let [kill (chan)]
    (go-loop [pending nil buffer nil]
      (alt!
        in
        ([edit]
         (if (nil? pending)
           (>! in* edit)
           (let [[pending' edit'] (transom/transform pending edit)]
             (if (nil? buffer)
               (do
                 (>! in* edit')
                 (recur pending' nil))
               (let [[buffer' edit''] (transom/transform buffer edit')]
                 (>! in* edit'')
                 (recur pending' buffer'))))))
        out
        ([edit]
         (>! out* edit)
         (if (nil? pending)
           (recur edit nil)
           (let [buffer' (if (nil? buffer)
                          edit
                          (transom/compose buffer edit))]
             (recur pending buffer'))))
        kill
        ([kill]
         (println "Goodbye. I love you baby."))))
    kill))

(defn websocket
  [inbox outbox]
  (let [ws (WebSocket. false)]
    (events/listen ws OPENED #(.send ws (pr-str {:type :init})))
    (events/listen ws CLOSED #(async/close! inbox))
    (events/listen ws ERROR #(do (println "ERROR" %) (async/close! inbox)))
    (events/listen ws MESSAGE (fn [ev]
                                (let [message (.-message ev)
                                      message (read-string message)]
                                  (put! inbox message))))
    (.open ws (str "ws://" (host) "/textarea"))
    (go-loop []
      (when-let [m (<! outbox)]
        (.send ws (pr-str m))
        (recur))
      (.close ws))))
(def fake-thing {:foo nil})
(println "Is this working?" (boolean (:foo fake-thing)))


;; Sync shouldn't deal with any parsing or anything like that right?
;; This is a dumb name and you should feel bad
(defn -main
  []
  (let [app-state! (atom {:doc nil :id nil :version nil})
        in    (chan)
        out   (chan)
        in*   (async/pub in :type)
        inits (async/sub in* :init (chan))
        edits (async/sub in* :edit (chan))
        wut   (async/sub in* nil (chan))]
    (websocket in out)
    (let [init
          (go (when-let [message (<! inits)]
            (swap! app-state! merge message)))])
    (go
      (<! init)
      (loop
        (when-let [message (<! edits)]
          (let [{:keys [edit]} message]
            )
          (recur))))
    (om/root
      app
      app-state!
      {:target (gdom/getElement "app")
       :tx-listen
       (fn [{:keys [old-value new-value path]} cursor]
         (let [{:keys [id last-seen]} @app-state!]
           (put! out {:id id
                      :last-seen last-seen
                      :type :edit
                      :edit (diff old-value new-value)})))})))
(-main)
