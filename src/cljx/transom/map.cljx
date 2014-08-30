(ns transom.map
  (:require [clojure.set :as set]
            [transom.utils :refer [key-set]]))

(defn diff
  [this that]
  (let [all-keys (set/union (key-set this) (key-set that))]
    (into {}
      (for [k all-keys]
        (let [old-v (get this k), new-v (get that k)]
          (cond
            (and (contains? this k) (not (contains? that k))) [k [:delete old-v]]
            (and (not (contains? this k)) (contains? that k)) [k [:insert new-v]]
            (not= old-v new-v) [k [:update old-v new-v]]))))))

(defn patch
  ([this edit]
    (reduce
      (fn [this [k [a b c]]]
        (case a
          :delete (dissoc this k)
          :update (assoc this k c)
          :insert (assoc this k b)))
      this
      edit))
  ([this edit & edits]
    (reduce patch (patch this edit) edits)))

(defn invert
  [edit]
  (into {}
    (map (fn [[k [a b c]]]
           (case a
             :delete [k [:insert b]]
             :insert [k [:delete b]]
             :update [k [:update c b]]))
         edit)))

(defn compose
  ([old new]
    (let [cmp (merge-with
                (fn [[a b c] [d e f]]
                  (case [a d]
                    ;[:insert :insert]
                    [:insert :update] [:insert f]
                    [:insert :delete] nil
                    ;[:update :insert]
                    [:update :update] [:update b f]
                    [:update :delete] [:delete b]
                    [:delete :insert] [:update b e]
                    ;[:delete :update]
                    ;[:delete :delete]
                    ))
                old
                new)]
      (into {} (remove (comp nil? val) cmp))))
  ([old new & more]
    (reduce compose (compose old new) more)))

(defn transform
  [mine yours]
  (let [shared-keys (set/intersection (key-set mine) (key-set yours))]
    (reduce
      (fn [[mine yours] k]
        (let [[a b c] (get mine k), [d e f] (get yours k)]
          (case [a d]
            [:insert :insert]
            (if (= b e)
              [(dissoc mine k) (dissoc yours k)]
              [(assoc mine k [:update e b]) (dissoc yours k)])
            ;[:insert :update]
            ;[:insert :delete]
            ;[:update :insert]
            [:update :update]
            (if (= c f)
              [(dissoc mine k) (dissoc yours k)]
              [(assoc mine k [:update f c]) (dissoc yours k)])
            [:update :delete] [(dissoc mine k) (assoc yours k [:delete c])]
            ;[:delete :insert]
            [:delete :update] [(assoc mine k [:delete f]) (dissoc yours k)]
            [:delete :delete] [(dissoc mine k) (dissoc yours k)])))
      [mine yours]
      shared-keys)))
