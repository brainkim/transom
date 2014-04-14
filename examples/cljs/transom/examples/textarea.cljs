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
  [data]
  (om/component
    (dom/pre nil
      (pr-str data))))

(defn app
  [data owner]
  (letfn [(change [e]
            (let [doc (.. e -target -value)]
              (om/update! data [:doc] doc)
              (om/update! data [:theirs] false)))
          (select [e]
            (let [node (.-target e)
                  start (selection/getStart node)
                  end (selection/getEnd node)]
              (om/update! data [:selection] {:start start :end end})))]
    (reify
      om/IShouldUpdate
      (should-update [_ props _]
        (not= (:doc data) (:doc props)))
      om/IDidUpdate
      (did-update [_ props _]
        (when (:theirs data)
          (let [node (om/get-node owner "textarea")]
            (selection/setStart node (get-in data [:selection :start]))
            (selection/setEnd node (get-in data [:selection :end])))))
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
                             :onChange change
                             :onSelect select}))))))

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
       [:+ (rand-char)]
       [:= (- doc-len entry)]])))

(defn fake-delete
  [doc-string]
  (let [doc-len (count doc-string)]
    (if (= 0 doc-len)
      [[:= doc-len]]
      (let [entry (rand-int (inc doc-len))
            delete-len (rand-int (inc (- doc-len entry)))]
        (transom/pack
          [[:= entry]
           [:- delete-len]
           [:= (- doc-len entry delete-len)]])))))

(defn mocksocket
  [in out _]
  (let [state (atom (document/document))]
    (go (while true
      (<! (timeout 1000))
      (swap! state
             (fn [doc]
               (let [edit (fake-delete (document/value doc))
                     doc (document/patch doc edit)
                     version (document/version doc)]
                 (put! in {:type :edit :id :FUCK_YOU :edit edit :version version})
                 doc)))))
    (go (while true
      (let [{:keys [edit version id]} (<! out)]
        (<! (timeout 32)) ;; simulate lag
        (swap! state
               (fn [doc]
                 (let [edit (document/transform-edit doc edit version)
                       doc  (document/patch doc edit)
                       version (document/version doc)]
                   (put! in {:type :edit :id id :edit edit :version version})
                   doc))))))))

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
  [in out conn !app-state !stage]
  (go (while true
    (alt!
      in
      ([message]
        (let [{:keys [edit version id]} message]
          (if (= id (:id @!app-state))
            ;; ack
            (swap! !stage
              (fn [{:keys [pending buffer] :as stage}]
                (if (nil? buffer)
                  {:pending nil :buffer nil :version version}
                  (do
                    (put! conn {:edit buffer :version version :id (:id @!app-state)})
                    {:pending buffer :buffer nil :version version}))))
            ;; external edit
            (let [{:keys [pending buffer]} @!stage]
              (swap! !stage assoc :version version)
              (if (nil? pending)
                (swap! !app-state
                       #(merge % {:doc (transom/patch (:doc %) edit)
                                  :theirs true
                                  :selection
                                  {:start (transom/transform-caret (get-in % [:selection :start]) edit)
                                   :end (transom/transform-caret (get-in % [:selection :end]) edit)}}))
                (let [[pending' edit'] (transom/transform pending edit)]
                  (if (nil? buffer)
                    (do
                      (swap! !stage assoc :pending pending')
                      (swap! !app-state
                             #(merge % {:doc (transom/patch (:doc %) edit')
                                        :theirs true
                                        :selection
                                        {:start (transom/transform-caret (get-in % [:selection :start])
                                                                         edit')
                                         :end (transom/transform-caret (get-in % [:selection :end])
                                                                       edit')}})))
                    (let [[buffer' edit''] (transom/transform buffer edit')]
                      (do
                        (swap! !stage merge {:pending pending' :buffer buffer'})
                        (swap! !app-state
                               #(merge % {:doc (transom/patch (:doc %) edit'')
                                          :theirs true
                                          :selection
                                          {:start (transom/transform-caret (get-in % [:selection :start])
                                                                           edit'')
                                           :end (transom/transform-caret (get-in % [:selection :end])
                                                                         edit'')}})))))))))))
      out
      ([edit]
        (swap! !stage
               (fn [{:keys [pending buffer version]}]
                 (if (nil? pending)
                   ;; immediately send edit
                   (do
                     (put! conn {:edit edit :version version :id (:id @!app-state)})
                     {:pending edit :buffer nil :version version})
                   ;; add edit to buffer
                   (if (nil? buffer)
                     {:pending pending :buffer edit :version version}
                     ;; buffer is full
                     {:pending pending
                      :buffer (transom/compose buffer edit)
                      :version version})))))))))

#_(defn stage
  [in out !stage]
  (go (while true
    (let [edit (<! in)]
      (swap! !stage
             (fn [{:keys [pending buffer version]}]
               (if (nil? pending)
                 ;; immediately send edit
                 (do
                   (put! out {:edit edit :version version :id (:id @!app-state)})
                   {:pending edit :buffer nil :version version})
                 ;; add edit to buffer
                 (if (nil? buffer)
                   {:pending pending :buffer edit :version version}
                   ;; buffer is full
                   {:pending pending
                    :buffer (transom/compose buffer edit)
                    :version version}))))))))

(defn ^:export main
  []
  (let [!app-state (atom {:doc "" :id 0 :selection {:start 0 :end 0}})
        !stage (atom {:pending nil :buffer nil :version nil})
        in (chan) out (chan) conn (chan)]
    (mocksocket in conn (WebSocket. false))
    (controller in out conn !app-state !stage)
    (om/root app !app-state
             {:target (gdom/getElement "app")
              :tx-listen
              (fn [{:keys [old-value new-value path]} cursor]
                (when (= path [:doc])
                  (put! out (diff old-value new-value))))})
    (om/root x-ray !stage {:target (gdom/getElement "xray")})))
(main)
