(ns transom.string
  (:require [transom.sequential :refer [pack-with
                                        diff-abstract
                                        patch-abstract
                                        compose-abstract
                                        transform-abstract]]))

(def pack-fn (partial apply str))

(def pack (pack-with pack-fn))

(defn diff [a b] (pack (diff-abstract a b)))

(defn patch
  ([doc edit] (pack-fn (patch-abstract doc edit)))
  ([doc edit & edits] (reduce patch (patch doc edit) edits)))

(defn compose
  ([edit1 edit2] (pack (compose-abstract edit1 edit2)))
  ([edit1 edit2 & more] (reduce compose (compose edit1 edit2) more)))

(defn transform [edit1 edit2] (map pack (transform-abstract edit1 edit2)))

(defn transform-caret
  [caret edit]
  ;; caret is the little blinky line thing
  ;; index is where we are in the (new?) document
  (first
    (reduce
      (fn [[caret index] [o p]]
        (case o
          :retain [caret, (+ index p)]
          :delete [(max (- caret p) 0), index]
          :insert [(if (>= caret index) (+ caret (count p)) caret), (+ index (count p))]))
      [caret, 0]
      edit)))
