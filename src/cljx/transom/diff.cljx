(ns transom.diff
  (:require [transom.core :as transom]))

(defn ^:private common-prefix
  [a b]
  (let [a-count (count a)
        b-count (count b)]
    (loop [i 0]
      (if (and (< i a-count) (< i b-count) (= (nth a i) (nth b i)))
        (recur (inc i))
        i))))

(defn ^:private common-suffix
  [a b]
  (let [a-count (count a)
        b-count (count b)]
    (loop [i 0]
      (if (and (< i a-count) (< i b-count)
               (= (nth a (- a-count i 1)) (nth b (- b-count i 1))))
        (recur (inc i))
        i))))

(defn ^:private create-edit
  [a b pre suf]
  (transom/pack [[:retain pre]
                 [:delete (- (count a) pre suf)]
                 [:insert (subs b pre (- (count b) suf))]
                 [:retain suf]]))

(defn diff
  [a b]
  (if (= a b)
    [[:retain (count a)]] ;; don't forget to wrap it dummy
    (let [pre (common-prefix a b)
          suf (common-suffix a b)]
      (if (< pre suf)
        (let [a' (subs a 0 (- (count a) suf))
              b' (subs b 0 (- (count b) suf))
              pre (common-prefix a' b')]
          (create-edit a b pre suf))
        (let [a' (subs a pre)
              b' (subs b pre)
              suf (common-suffix a' b')]
          (create-edit a b pre suf))))))
