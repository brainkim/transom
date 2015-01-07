(ns transom.string
  (:require [clojure.string :as str]
            [transom.sequential :as ts]))

(def pack-fn str/join)

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

(def transform-caret ts/transform-index)
