(ns transom.vector
  (:require [transom.sequential :as ts]))

(def pack-fn vec)

(def pack (ts/pack-with pack-fn))

(defn diff [a b] (pack (ts/diff-abstract a b)))

(defn patch
  ([doc edit] (pack-fn (ts/patch-abstract doc edit)))
  ([doc edit & edits] (reduce patch (patch doc edit) edits)))

(defn compose
  ([edit1 edit2] (pack (ts/compose-abstract edit1 edit2)))
  ([edit1 edit2 & more] (reduce compose (compose edit1 edit2) more)))

(defn transform [edit1 edit2] (map pack (ts/transform-abstract edit1 edit2)))

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
