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
            [transom.examples.websocket :refer [socket]]))
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

(defn send-edit!
  [sock edit version id]
  (put! sock (pr-str {:type :edit :edit edit :version version :id id})))

(defn stage
  [sock edit !app-state !stage]
  (let [id (get @!app-state :id)]
    (swap! !stage
      (fn [{:keys [pending buffer version]}]
        (if (nil? pending)
          (do
            (send-edit! sock edit version id)
            {:version version
             :pending edit
             :buffer nil})
          (if (nil? buffer)
            {:version version
             :pending pending
             :buffer edit}
            {:version version
             :pending pending
             :buffer (transom/compose buffer edit)}))))))

(defn acknowledge
  [sock version id !stage]
  (swap! !stage
    (fn [{past-version :version :keys [pending buffer]}]
      (assert (not (nil? pending))
              "Received ack without anything pending")
      (if (nil? buffer)
        {:pending nil :buffer nil :version version}
        (do
          (send-edit! sock buffer past-version id)
          {:pending buffer :buffer nil :version version})))))

(defn patch
  [edit !app-state]
  (swap! !app-state
    (fn [{:keys [doc selection]}]
      {:doc (transom/patch doc edit)
       :theirs true
       :selection
       {:start (transom/transform-caret (:start selection) edit)
        :end   (transom/transform-caret (:end selection) edit)}})))

(defn transform
  [edit version !app-state !stage]
  (swap! !stage
    (fn [{:keys [pending buffer]}]
      (if (nil? pending)
        (do
          (patch edit !app-state)
          {:version version :pending nil :buffer nil})
        (let [[pending' edit'] (transom/transform pending edit)]
          (if (nil? buffer)
            (do
              (patch edit' !app-state)
              {:version version :pending 'pending :buffer nil})
            (let [[buffer' edit''] (transom/transform buffer edit')]
              (patch edit'' !app-state)
              {:version version :pending pending' :buffer 'buffer})))))))

(defn receive
  [sock !app-state !stage]
  (go (while true
    (when-let [message (<! sock)]
      (let [{:keys [edit version id]} (read-string message)]
        (if (= id (:id @!app-state))
          (acknowledge sock version id !stage)
          (transform edit version !app-state !stage)))))))

(defn ^:export main
  []
  (let [!app-state (atom {:doc "" :id 0 :selection {:start 0 :end 0}})
        !stage (atom {:pending nil :buffer nil :version 0})
        sock (socket (str "ws://" (host) "/textarea"))]
    (receive sock !app-state !stage)
    (om/root app !app-state
             {:target (gdom/getElement "app")
              :tx-listen
              (fn [{:keys [old-value new-value path]} cursor]
                (when (= path [:doc])
                  (stage sock (diff old-value new-value) !app-state !stage)))})
    (om/root x-ray !stage
             {:target (gdom/getElement "xray")})))
(main)
