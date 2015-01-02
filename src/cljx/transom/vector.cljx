(ns transom.vector
  (:require [transom.sequential :refer [pack-with
                                        diff-abstract
                                        patch-abstract
                                        compose-abstract
                                        transform-abstract]]))

(def pack-fn vec)

(def pack (pack-with pack-fn))

(defn diff [a b] (pack (diff-abstract a b)))

(defn patch
  ([doc edit] (pack-fn (patch-abstract doc edit)))
  ([doc edit & edits] (reduce patch (patch doc edit) edits)))

(defn compose
  ([edit1 edit2] (pack (compose-abstract edit1 edit2)))
  ([edit1 edit2 & more] (reduce compose (compose edit1 edit2) more)))

(defn transform [edit1 edit2] (map pack (transform-abstract edit1 edit2)))

(defn transform-key
  [key edit destructive?]
  (loop [key key, index 0, edit edit]
    (if-let [[o p] (first edit)]
      (case o
        :retain
        (recur key (+ index p) (rest edit))

        :delete
        (if (>= key index)
          (cond
            (>= (- key index) p) (recur (- key p) index (rest edit))
            (not destructive?) 0)
          (recur key index (rest edit)))

        :insert
        (let [p (count p)]
          (if (>= key index)
            (recur (+ key p) (+ index p) (rest edit))
            (recur key (+ index p) (rest edit)))))
      key)))
