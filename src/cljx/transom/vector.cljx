(ns transom.vector
  (:require [transom.sequential :as ts]))

(def pack-fn vec)

(def pack (partial ts/pack-with pack-fn))

(defn diff [a b] (pack (ts/diff-abstract a b)))

(defn patch
  ([doc edit] (pack-fn (ts/patch-abstract doc edit)))
  ([doc edit & edits] (reduce patch (patch doc edit) edits)))

(defn invert [edit] (pack (ts/invert-abstract edit)))

(defn compose
  ([edit1 edit2] (pack (ts/compose-abstract edit1 edit2)))
  ([edit1 edit2 & more] (reduce compose (compose edit1 edit2) more)))

(defn transform [edit1 edit2] (map pack (ts/transform-abstract edit1 edit2)))

(defn transform-key
  [k edit destructive?]
  (loop [k k, i 0, edit edit]
    (if-let [[o p] (first edit)]
      (case o
        :retain
        (recur k (+ i p) (rest edit))

        :delete
        (let [p (count p)]
          (if (>= k i)
            (cond
              (not destructive?) 0
              (>= (- k i) p) (recur (- k p) i (rest edit))
              :else nil)
            (recur k i (rest edit))))

        :insert
        (let [p (count p)]
          (if (>= k i)
            (recur (+ k p) (+ i p) (rest edit))
            (recur k (+ i p) (rest edit)))))
      k)))
