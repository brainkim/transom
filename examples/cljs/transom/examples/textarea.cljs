(ns transom.examples.textarea
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [cljs.core.async :as async :refer [put! chan >! <! timeout]]
            [cljs.reader :refer [read-string]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [transom.core :as transom]
            [transom.document :as document]
            [goog.dom :as gdom]
            [goog.dom.selection :as selection]
            [transom.examples.websocket :refer [socket]]))
(comment
(enable-console-print!)

(def ^:dynamic *sock* nil)
(def ^:dynamic *id* nil)

(defn host
  []
  (.. js/window -location -host))

(defn initialize
  [inits !app-state !stage]
  (go
    (when-let [{:keys [id doc version] :as message} (<! inits)]
      (set! *id* id)
      (swap! !app-state assoc :doc doc)
      (swap! !stage assoc :version version)
      message)))

(defn send-edit!
  [edit version]
  (put! *sock* (pr-str {:type :edit :edit edit :version version :id *id*})))

(defn stage
  [edit !stage]
  (swap! !stage
    (fn [{:keys [pending buffer version]}]
      (if (nil? pending)
        (do
          (send-edit! edit version)
          {:version version :pending edit :buffer nil})
        (if (nil? buffer)
          {:version version :pending pending :buffer edit}
          {:version version :pending pending
           :buffer (transom/compose buffer edit)})))))

(defn acknowledge
  [edit version !stage]
  (swap! !stage
    (fn [{:keys [pending buffer]}]
      (assert (not (nil? pending)) "Received ack without anything pending")
      (if (nil? buffer)
        {:pending nil :buffer nil :version version}
        (do
          ;; we can send the new version because we don't have to transform against our own edits
          (send-edit! buffer version)
          {:pending buffer :buffer nil :version version})))))

(defn patch
  [edit !app-state]
  (swap! !app-state
    (fn [{:keys [doc selection]}]
      {:doc (transom/patch doc edit)
       :theirs true
       :selection
       {:start (transom/transform-caret (:start selection) edit)
        :end (transom/transform-caret (:end selection) edit)}})))

(defn transform
  [edit version !app-state !stage]
  (swap! !stage
    (fn [{:keys [pending buffer]}]
      (if (nil? pending)
        (do
          (patch edit !app-state)
          {:version version :pending nil :buffer nil})
        (let [[edit' pending'] (transom/transform edit pending)]
          (if (nil? buffer)
            (do
              (patch edit' !app-state)
              {:version version :pending pending' :buffer nil})
            (let [[edit'' buffer'](transom/transform edit' buffer)]
              (patch edit'' !app-state)
              {:version version :pending pending' :buffer buffer'})))))))

(defn receive
  [edits !app-state !stage]
  (go
    (when-let [message (<! edits)]
      (let [{:keys [edit version id]} message]
        (if (= id *id*)
          (acknowledge edit version !stage)
          (transform edit version !app-state !stage)))
      ;; Force rerender. `render-all` is a private var
      (om/render-all)
      (receive edits !app-state !stage))))

(defn edit-distance
  [edit]
  (letfn
    [(count-op
       [[o p]]
       (case o
         :insert (count p)
         :delete p
         :retain 0))]
    (reduce (fn [prev op] (+ prev (count-op op))) 0 edit)))

(defn x-ray
  [data]
  (om/component
    (dom/pre nil
      (pr-str data))))

(defn collab-textarea
  [data owner]
  (letfn
    [(change [ev]
       (let [doc (.. ev -target -value)]
         (om/update! data [:doc] doc)
         (om/update! data [:theirs] false)))
     (select [ev]
       (let [node (.-target ev)
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
          (dom/textarea #js {:disabled (nil? (:doc data))
                             :value (or (:doc data) "")
                             :ref "textarea"
                             :autoFocus true
                             :id "textarea"
                             :rows 50
                             :cols 80
                             :onChange change
                             :onSelect select}))))))

(defn app
  [data owner]
  (om/component
    (dom/div nil
      (om/build x-ray data)
      (om/build collab-textarea data))))

((defn main
  []
  (set! *sock* (socket (str "ws://" (host) "/ws/textarea")))
  (let [!app-state (atom {:doc nil :selection {:start 0 :end 0}})
        !stage (atom {:pending nil :buffer nil :version nil})
        sock-pub (async/pub (async/map< (fn [message]
                                          (println message)
                                          (read-string message))
                                        *sock*) :type)
        inits (async/sub sock-pub :init (chan))
        edits (async/sub sock-pub :edit (chan))]
    (initialize inits !app-state !stage)
    (receive edits !app-state !stage)
    (om/root app !app-state
             {:target (gdom/getElement "app")
              :tx-listen
              (fn [{:keys [old-value new-value path]} cursor]
                (when (= path [:doc])
                  (let [diff (transom/diff old-value new-value)
                        ed (edit-distance diff)]
                    (stage diff !stage))))})
    (om/root x-ray !stage {:target (gdom/getElement "xray")}))))
)
