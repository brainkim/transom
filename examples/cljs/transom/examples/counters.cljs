(ns transom.examples.counters
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [transom.core :as transom]))
(enable-console-print!)

(defn x-ray
  [data owner]
  (om/component
    (dom/pre nil (pr-str data))))

(defn counter
  [data owner {:keys [parent-data]}]
  (letfn
    [(update [ev]
       (om/update! data [:value] (.. ev -target -value)))
     (delete [_]
       (om/transact! parent-data []
         (fn [counters] (filterv #(not= (get % :id) (get @data :id)) counters))))]
    (reify
      om/IRender
      (render [_]
        (dom/div nil
          (dom/input #js {:value (data :value)
                          :onChange update})
          (dom/button #js {:onClick delete} "x"))))))

(defn counter-view
  [data owner]
  (letfn
    [(create [_]
       (om/transact! data [:counters]
                     #(conj % {:id (-> % last :id inc) :value "grrr"})))]
    (reify
      om/IRender
      (render [_]
        (apply dom/div nil
          (dom/h1 #js {:key "head"} "Counters")
          (dom/button #js {:key "add" :onClick create} "Add")
          (om/build-all counter (get data :counters)
                        {:key :id
                         :opts {:parent-data (get data :counters)}}))))))

(def app-state {:counters (mapv (fn [n] {:id n :value ""}) (range 10))})
(def !app-state (atom app-state))
(def !composed (atom {}))

(defn tx-listener
  [{:keys [path old-value new-value old-state new-state]}]
  (let [edit-map {path (transom/diff old-value new-value)}
        state (transom/patch old-state edit-map)]
    (swap! !composed #(transom/compose old-state % edit-map))
    (let [complete-state (transom/patch app-state @!composed)]
      (println (pr-str complete-state))
      (assert (= new-state state complete-state)
              (str (pr-str new-state) \newline
                   (pr-str state) \newline
                   (pr-str complete-state))))))

(om/root
  counter-view
  !app-state
  {:target (.getElementById js/document "app")
   :tx-listen tx-listener})

(om/root
  x-ray
  !app-state
  {:target (.getElementById js/document "xray")})
