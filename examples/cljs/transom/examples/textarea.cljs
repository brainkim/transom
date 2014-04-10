(ns transom.examples.textarea
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [cljs.core.async :as async :refer [put! chan >! <! timeout]]
            [cljs.reader :refer [read-string]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [transom.core :as transom]
            [transom.diff :refer [diff]]
            [transom.document :as document]
            [goog.dom :as gdom]
            [goog.dom.selection :as selection]
            [goog.events :refer [listen]]
            [goog.net.WebSocket.EventType :refer [OPENED CLOSED ERROR MESSAGE]])
  (:import (goog.net WebSocket)))
(enable-console-print!)

(defn host
  []
  (.. js/window -location -host))

(defn x-ray
  [data owner]
  (om/component
    (dom/pre nil
      (pr-str data))))

(defn app
  [data owner]
  (letfn [(change [e]
            (let [message (.. e -target -value)]
              (om/update! data [:doc] message)))]
    (reify
      om/IDidUpdate
      (did-update [_ props state]
        (let [node (om/get-node owner "textarea")]
          nil))
      om/IRender
      (render [_]
        (dom/div nil
          (om/build x-ray data)
          (dom/textarea #js {:disabled (nil? (:id data))
                             :value (:doc data)
                             :ref "textarea"
                             :autoFocus true
                             :rows 50
                             :cols 80
                             :onChange change}))))))

(defn rand-char
  []
  (let [chars (map char (concat (range 48 58) (range 66 92) (range 97 123)))]
    (rand-nth chars)))

(defn rand-edit
  [doc-string]
  (transom/pack
    (let [doc-len (count doc-string)
          entry (rand-int doc-len)]
      [[:= entry]
       [:+ "pizza"]
       [:= (- doc-len entry)]])))

(defn start-pizza
  [doc-string]
  [[:+ "pizza"]
   [:= (count doc-string)]])

(defn end-edit
  [doc-string]
  [[:= (count doc-string)]
   [:+ (take 5 (repeatedly rand-char))]])

(defn mocksocket
  [in out _]
  (let [state (atom (document/document))]
    #_(go-loop []
      (<! (timeout 5000))
      (swap! state
             (fn [doc]
               (let [edit (start-pizza (document/value doc))
                     doc (document/patch doc edit)
                     version (document/version doc)]
                 (put! in {:type :edit :id :FUCK_YOU :edit edit :version version})
                 doc)))
      (recur))
    (go-loop []
      (let [{:keys [edit version]} (<! out)]
        (<! (timeout 5000))
        (swap! state
               (fn [doc]
                 (let [edit (document/transform-edit doc edit version)
                       doc  (document/patch doc edit)
                       version (document/version doc)]
                   (put! in {:type :edit :id 0 :edit edit :version version})
                   doc))))
      (recur))))

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
  [in out conn app-state! stage!]
  (go-loop []
    (alt!
      in
      ([message]
        (let [{:keys [edit version id]} message]
          (if (not= id 0)
            ;; external edit
            (let [{:keys [pending buffer]} @stage!]
              (swap! stage! assoc :version version)
              (if (nil? pending)
                (swap! app-state! #(merge % {:doc (transom/patch (:doc %) edit)}))
                (let [[pending' edit'] (transom/transform pending edit)]
                  (if (nil? buffer)
                    (do 
                      (swap! stage! assoc :pending pending')
                      (swap! app-state!
                             #(merge % {:doc (transom/patch (:doc %) edit')})))
                    (let [[buffer' edit''] (transom/transform buffer edit')]
                      (do
                        (swap! stage! merge {:pending pending' :buffer buffer'})
                        (swap! app-state!
                               #(merge % {:doc (transom/patch (:doc %) edit'')}))))))))
            ;; ack
            (swap! stage!
              (fn [{:keys [pending buffer] :as stage}]
                (if (nil? buffer)
                  {:pending nil :buffer nil :version version}
                  (do
                    (put! conn {:edit buffer :version version})
                    {:pending buffer :buffer nil :version version}))))))
        (recur))
      out
      ([edit]
        (swap! stage!
               (fn [{:keys [pending buffer version]}]
                 (if (nil? pending)
                   (do
                     ;; immediately send message
                     (put! conn {:edit edit :version version})
                     {:pending edit :buffer nil :version version})
                   ;; pending message
                   (if (nil? buffer)
                     {:pending pending :buffer edit :version version}
                     ;; buffer is full
                     {:pending pending
                      :buffer (transom/compose buffer edit)
                      :version version}))))
        (recur)))))

(defn -main
  []
  (letfn [(patch-doc
            [app-state edit]
            (assoc-in app-state :doc (transom/patch (:doc app-state) edit)))]
    (let [app-state! (atom {:doc "" :id 0})
          stage! (atom {:pending nil :buffer nil :version nil})
          in (chan)
          out (chan)
          out* (chan)]
      (mocksocket in out* nil)
      (controller in out out* app-state! stage!)
      (om/root app
               app-state!
               {:target (gdom/getElement "app")
                :tx-listen
                (fn [{:keys [old-value new-value path]} cursor]
                  (println (diff old-value new-value))
                  (put! out (diff old-value new-value)))})
      (om/root x-ray stage! {:target (gdom/getElement "xray")}))))
(-main)
