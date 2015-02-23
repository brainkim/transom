(ns transom.examples.counters
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [transom.core :as transom]
            [transom.protocols :as impl]))
(enable-console-print!)

(defn ^:private rand-char [] ((vec (map char (range 97 123))) (rand-int 26)))

(def app-state {:counters
                (mapv (fn [n] {:id n :value (apply str (repeat n "f"))})
                      (range 10))})
(def !app-state (atom app-state))

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

(defn counter-list
  [data owner]
  (letfn
    [(create [_]
       (om/transact! data [:counters]
                     #(conj % {:id (-> % last :id inc)
                               :value (apply str (repeat (inc (rand-int 5)) "g"))})))
     (reset [_]
       (om/update! data [] app-state))]
    (reify
      om/IRender
      (render [_]
        (apply dom/div nil
          (dom/div #js {}
            (dom/h1 #js {:key "head"} "Counters")
            (dom/button #js {:key "add" :onClick create} "Add")
            (dom/button #js {:key "reset" :onClick reset} "Reset"))
          (om/build-all counter (get data :counters)
                        {:opts {:parent-data (get data :counters)}}))))))

(def !composed (atom {}))
(def concurrent {[:counters] [[:delete 5] [:retain 5]]
                 [:counters 0 :value] [[:retain 5] [:insert "hey!"]]
                 [:counters 4 :value] [[:retain 9]]})

(defn tx-listener
  [{:keys [path old-value new-value old-state new-state]}]
  (let [diff {path (transom/diff old-value new-value)}
        new-state' (transom/patch old-state diff)]
    (println (pr-str @!composed) \newline
             (pr-str diff))
    (swap! !composed #(transom/compose app-state % diff))
    (println (pr-str @!composed))
    (let [composed @!composed
          new-state'' (transom/patch app-state composed)]
      (assert (= new-state new-state' new-state'')
              (str \newline (pr-str new-state)
                   \newline (pr-str new-state')
                   \newline (pr-str new-state'')
                   \newline "composed" composed))
      (let [[concurrent' composed'] (transom/transform app-state concurrent composed)]
        (assert (= (transom/patch app-state composed concurrent')
                   (transom/patch app-state concurrent composed'))
                (str \newline (pr-str composed)
                     \newline (pr-str concurrent)
                     \newline (pr-str composed')
                     \newline (pr-str concurrent')))))))

(om/root
  counter-list
  !app-state
  {:target (.getElementById js/document "app")
   :tx-listen tx-listener})

(om/root
  x-ray
  !app-state
  {:target (.getElementById js/document "xray")})
