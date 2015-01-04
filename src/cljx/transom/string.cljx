(ns transom.string
  (:require [clojure.string :as str]
            [transom.sequential :as ts]))

(def pack-fn str/join)

(def pack (partial ts/pack-with pack-fn))

(defn diff [a b] (pack (ts/diff-abstract a b)))

(defn patch
  ([doc edit] (pack-fn (ts/patch-abstract doc edit)))
  ([doc edit & edits] (reduce patch (patch doc edit) edits)))

(defn invert
  [edit]
  (pack (ts/invert-abstract edit)))

(defn compose
  ([edit1 edit2] (pack (ts/compose-abstract edit1 edit2)))
  ([edit1 edit2 & more] (reduce compose (compose edit1 edit2) more)))

(defn transform
  [edit1 edit2]
  (map pack (ts/transform-abstract edit1 edit2)))

(defn transform-caret
  [caret edit]
  ;; caret is the little blinky line thing
  ;; index is where we are in the (new?) document
  (first
    (reduce
      (fn [[caret index] [o p]]
        (case o
          :retain [caret, (+ index p)]
          :delete [(max (- caret (count p)) 0), index]
          :insert [(if (>= caret index) (+ caret (count p)) caret), (+ index (count p))]))
      [caret 0]
      edit)))
