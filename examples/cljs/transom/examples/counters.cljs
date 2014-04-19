(ns transom.examples.counters
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan put! sliding-buffer]]))
(enable-console-print!)

(defn counter
  [data owner {:keys [parent]}]
  (reify
    om/IRender
    (render [_]
      (letfn
        [(inc-click [_]
           (om/transact! data [:count] inc))
         (dec-click [_]
           (om/transact! data [:count] dec))
         (delete-click [_]
           (om/transact! parent [:counters]
             (fn [counters] (into [] (remove #(= (:id %) (:id @data)) counters)))))]
        (dom/div nil
          (dom/div nil
            (str (:id data) " -> " (:count data)))
          (dom/button #js {:onClick inc-click} "+")
          (dom/button #js {:onClick dec-click} "-")
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
          (om/build-all counter (:counters data) {:key :id
                                                  :opts {:parent data}}))))))

(def app-state (atom {:counters (into [] (map (fn [n] {:id n :count 0}) (range 10)))}))

(om/root counter-view app-state
  {:target (.getElementById js/document "app")
   :tx-listen (fn [{:keys [path old-value new-value new-state]} cursor]
                (println new-state))})
