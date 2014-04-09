(ns transom.examples.textarea
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [cljs.core.async :as async :refer [put! chan >! <! timeout]]
            [cljs.reader :refer [read-string]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [transom.core :as transom]
            [goog.dom :as gdom]
            [goog.dom.selection :as selection]
            [goog.events :refer [listen]]
            [goog.net.WebSocket.EventType :refer [OPENED CLOSED ERROR MESSAGE]])
  (:import (goog.net WebSocket)))
(enable-console-print!)

(defn host
  []
  (.. js/window -location -host)) 

(defn app
  [data owner]
  (letfn [(change [e]
            (let [message (.. e -target -value)]
              (om/update! data [:doc] message)))]
    (reify
      om/IWillUpdate
      (will-update [_ props state]
        (let [node (.. owner -refs -textarea getDOMNode)]
          (selection/setStart node 0)
          (selection/setEnd node 0)))
      om/IRender
      (render [_]
        (dom/div nil
          (dom/textarea #js {:disabled (nil? (:version data))
                             :value (:doc data)
                             :ref "textarea"
                             :autoFocus true
                             :rows 50
                             :cols 80
                             :onChange change}
          (dom/div nil (str "Data: " (pr-str data)))))))))

(defn diff
  [a b]
  (let [a-len (count a)
        b-len (count b)]
    (letfn
      [(common-prefix [a b]
         (loop [i 0]
           (if (and (< i a-len) (< i b-len) (= (.charAt a i) (.charAt b i)))
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

(defn rand-char
  []
  (let [chars (map char (concat (range 48 58) (range 66 92) (range 97 123)))]
    (rand-nth chars)))

(defn rand-edit
  [doc]
  (transom/pack
    (let [doc-len (count doc)
          entry (rand-int doc-len)]
      [[:= entry]
       [:+ "pizza"]
       [:= (- doc-len entry)]])))

(defn mocksocket
  [in out ws]
  (go-loop []
    (println (<! out))
    (recur))
  #_(go-loop [doc "" version 0]
    (<! (timeout 1500))
    (let [edit (rand-edit doc)
          doc (transom/patch doc edit)]
      (>! in {:type :edit
              :id :FUCK_YOU
              :edit edit
              :version version})
      (recur doc (inc version)))))

(defn websocket
  [in out ws]
  (listen ws OPENED 
          (fn [_]
            (.send ws (pr-str {:type :init}))))
  (listen ws CLOSED
          (fn [_]
            (async/close! in)))
  (listen ws ERROR
          (fn [ev]
            (println "ERROR" ev)
            (async/close! in)))
  (listen ws MESSAGE
          (fn [ev]
            (let [message (.-message ev)
                  message (read-string message)]
              (put! in message))))
  (.open ws (str "ws://" (host) "/textarea"))
  (go-loop []
    (when-let [m (<! out)]
      (.send ws (pr-str m))
      (recur))
    (.close ws)))

(defn controller
  [in out out* app-state! stage!]
  (go-loop []
    (alt!
      in
      ([message]
        (println @stage!)
        (if (not= (:id message) (:id @app-state!))
          (let [{:keys [pending buffer]} @stage!
                {:keys [version edit]} message]
            (if (nil? pending)
              (swap! app-state!
                     #(merge {:doc (transom/patch (:doc %) edit)
                              :version version}))
              (let [[pending' edit'] (transom/transform pending edit)]
                (if (nil? buffer)
                  (do 
                    (swap! stage! assoc :pending pending')
                    (swap! app-state!
                           #(merge {:doc (transom/patch (:doc %) edit')
                                    :version version})))
                  (let [[buffer' edit''] (transom/transform buffer edit')]
                    (do
                      (swap! stage! merge {:pending pending' :buffer buffer'})
                      (swap! app-state!
                             #(merge {:doc (transom/patch (:doc %) edit'')
                                      :version version})))))))))
        (recur))
      out
      ([message]
        (let [{:keys [edit]} message]
          (swap! stage! (fn [{:keys [pending buffer]}]
                 (if (nil? pending)
                   (do
                     (put! out* edit)
                     {:pending edit
                      :buffer nil})
                   (if (nil? buffer)
                     {:pending pending
                      :buffer edit}
                     {:pending pending
                      :buffer (transom/compose buffer edit)})))))
        (recur)))))

(defn -main
  []
  (letfn [(patch-doc
            [app-state edit]
            (assoc-in app-state :doc (transom/patch (:doc app-state) edit)))]
    (let [app-state! (atom {:doc "" :id nil :version 0})
          stage! (atom {:pending nil :buffer nil})
          in (chan)
          out (chan) out* (chan)]
      (mocksocket in out* (WebSocket. false))
      (controller in out out* app-state! stage!)
      (om/root app
        app-state!
        {:target (gdom/getElement "app")
         :tx-listen
         (fn [{:keys [old-value new-value path]} cursor]
           (let [{:keys [id version]} @app-state!]
             (put! out {:type :edit
                        :id id
                        :path path
                        :edit (diff old-value new-value)})))}))))
(-main)
