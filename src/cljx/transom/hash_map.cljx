(ns transom.hash-map
  (:require [clojure.set :as set]))

(def remove-keys (partial apply dissoc))

(defn pack
  [edit]
  ;; TODO
  )

(defn diff
  [this that]
  (let [this-keys (set (keys this))
        that-keys (set (keys that))
        inserts (set/difference that-keys this-keys)
        deletes (set/difference this-keys that-keys)
        updates (set/intersection this-keys that-keys)]
    {:inserts (select-keys that inserts)
     :deletes (select-keys this deletes)
     :updates (merge-with (fn [this that] [this that])
                          (select-keys this updates)
                          (select-keys that updates))}))

(defn patch
  [this {:keys [inserts deletes updates]}]
  (let [this (merge this inserts)
        this (merge-with (fn [_ new] (second new)) this updates)]
    (remove-keys this (keys deletes))))

(defn compose
  [old new]
  (let [{old-updates :updates
         old-inserts :inserts
         old-deletes :deletes} old
        {new-updates :updates
         new-inserts :inserts
         new-deletes :deletes} new
        old-updates (remove-keys old-updates (keys new-deletes))
        old-inserts (remove-keys old-inserts (keys new-deletes))
        old-deletes (remove-keys old-deletes
                                 (concat (keys new-updates) (keys new-inserts)))]
    {:updates (merge old-updates new-updates)
     :inserts (merge old-inserts new-inserts)
     :deletes (merge old-deletes new-deletes)}))

(defn transform
  [mine yours]
  ;; TODO
  )
