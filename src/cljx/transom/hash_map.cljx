(ns transom.hash-map
  (:require [clojure.set :as set]))

(def remove-keys (partial apply dissoc))

(defn pack
  [edit]
  (into {}
    (for [[k v] edit]
      (if (empty? v)
        nil
        [k v]))))

(defn diff
  [this that]
  (let [this-keys (set (keys this))
        that-keys (set (keys that))
        i-keys (set/difference that-keys this-keys)
        d-keys (set/difference this-keys that-keys)
        u-keys (set/intersection this-keys that-keys)]
    {:inserts (select-keys that i-keys)
     :deletes (select-keys this d-keys)
     :updates (merge-with #(vector %1 %2)
                          (select-keys this u-keys)
                          (select-keys that u-keys))}))

(defn patch
  [this {:keys [inserts deletes updates]}]
  (doseq [k (keys inserts)]  (assert (not (contains? this k))))
  (doseq [[k v] deletes]     (assert (= (get this k) v)))
  (doseq [[k [v _]] updates] (assert (= (get this k) v)))
  (let [this (merge this inserts)
        this (merge-with #(second %2) this updates)]
    (remove-keys this (keys deletes))))

(defn compose
  [old new]
  (let [{old-i :inserts
         old-u :updates
         old-d :deletes} old
        {new-i :inserts
         new-u :updates
         new-d :deletes} new
        old-i' (remove-keys old-i (keys new-d))
        old-u' (remove-keys old-u (keys new-d))
        new-u' (remove-keys new-u (keys old-i))
        new-u'' (into {} (map #(vector (key %) (second (val %))) (select-keys new-u (keys old-i))))
        old-d' (remove-keys old-d (concat (keys new-i) (keys new-u)))]
    (pack {:inserts (merge old-i' new-i new-u'')
           :updates (merge-with #(vector (first %1) (second %2)) old-u' new-u')
           :deletes (merge old-d' new-d)})))

(defn transform
  [mine yours]
  ;; TODO
  )
