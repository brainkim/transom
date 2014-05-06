(ns transom.examples.counters
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan put! sliding-buffer]]
            [transom.core :as transom]
            [transom.string :as tstr]
            [transom.indexed :as indexed]))
(enable-console-print!)

(defn x-ray
  [data owner]
  (om/component
    (dom/pre nil (pr-str data))))

(defn counter
  [data owner {:keys [parent]}]
  (reify
    om/IRender
    (render [_]
      (letfn
        [(change [ev]
           (om/update! data [:value] (.. ev -target -value)))
         (delete-click [_]
           (om/transact! parent []
             (fn [counters] (into [] (remove #(= (:id %) (:id @data)) counters)))))]
        (dom/div nil
          (dom/input #js {:value (data :value)
                          :onChange change})
          (dom/button #js {:onClick delete-click} "x"))))))

(defn counter-view
  [data owner]
  (letfn
    [(add-click [_]
       (om/transact! data :counters #(conj % {:id (-> % last :id inc) :count 0})))]
    (reify
      om/IRender
      (render [_]
        (apply dom/div nil
          (dom/h1 #js {:key "head"} "Counters")
          (dom/button #js {:key "add" :onClick add-click} "Add")
          (om/build-all counter (data :counters) {:key :id
                                                  :opts {:parent (data :counters)}}))))))

(def app-state (atom {:counters (vec (map (fn [n] {:id n :value ""}) (range 10)))}))

(om/root counter-view app-state
  {:target (.getElementById js/document "app")
   :tx-listen (fn [{:keys [path old-value new-value]}]
                (println path))})

(om/root x-ray app-state {:target (.getElementById js/document "xray")})
