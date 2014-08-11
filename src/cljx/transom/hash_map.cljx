(ns transom.hash-map
  (:require [clojure.set :as set]))

(def ^:private key-set (comp set keys))

(def ^:private remove-keys (partial apply dissoc))

(defn ^:private filter-updates
  [edit]
  (if-let [updates (:updates edit)]
    (assoc edit :updates
           (select-keys updates (for [[k [v1 v2]] updates :when (not= v1 v2)] k)))
    edit))

(defn pack
  [edit]
  (let [edit (filter-updates edit)]
    (into {} (for [[k v] edit] (if (empty? v) nil [k v])))))

(defn ^:private venn
  [set-1 set-2]
  [(set/difference set-1 set-2)
   (set/intersection set-1 set-2)
   (set/difference set-2 set-1)])

(defn diff
  [this that]
  (let [[d-keys u-keys i-keys] (venn (key-set this) (key-set that))]
    (pack {:deletes (select-keys this d-keys)
           :updates (merge-with vector
                                (select-keys this u-keys)
                                (select-keys that u-keys))
           :inserts (select-keys that i-keys)})))

(defn patch
  [this {:keys [inserts updates deletes]}]
  (doseq [k (keys inserts)]  (assert (not (contains? this k))))
  (doseq [[k [v _]] updates] (assert (= (get this k) v)))
  (doseq [[k v] deletes]     (assert (= (get this k) v)))
  (let [this (merge this inserts)
        this (merge-with #(second %2) this updates)]
    (remove-keys this (keys deletes))))

(defn ^:private update-insert-map
  [updates]
  (into {} (for [[k [_ v2]] updates] [k v2])))

(defn compose
  [old new]
  (let [{old-d :deletes
         old-u :updates
         old-i :inserts} old
        {new-d :deletes
         new-u :updates
         new-i :inserts} new
        old-d' (remove-keys old-d (concat (keys new-i) (keys new-u))) 
        old-u' (remove-keys old-u (keys new-d))
        new-u' (remove-keys new-u (keys old-i)) 
        new-ui (select-keys (update-insert-map new-u) (keys old-i)) 
        old-i' (remove-keys old-i (keys new-d))]
    (pack {:deletes (merge old-d' new-d)    
           :updates (merge-with #(vector (first %1) (second %2)) old-u' new-u')
           :inserts (merge old-i' new-i new-ui)})))

(defn transform
  [mine yours]
  (let [{my-d :deletes
         my-u :updates
         my-i :inserts} mine
        {your-d :deletes
         your-u :updates
         your-i :inserts} yours
        our-i-keys (set/intersection (key-set my-i) (key-set your-i))
        my-i ()
        your-i (if (seq our-i-keys)
                 (remove-keys your-i our-i-keys)
                 your-i)]

    ))
