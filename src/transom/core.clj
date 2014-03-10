(ns transom.core
  (:require [clojure.core.match :refer [match]]))

(defn count-op
  [op]
  (let [f (fn [c sop]
            (match [sop]
              [[:= v]] (+ c v)
              [[:- v]] (+ c v)
              [[:+ v]] c))]
    (reduce f 0 op)))

(defn apply-op
  [doc op]
  (assert (= (count doc) (count-op op)))
  (loop [doc doc doc' nil op op]
    (match [op]
      [([] :seq)] (apply str doc')
      [([[:= v] & r] :seq)] (recur (drop v doc) (concat doc' (take v doc)) r)
      [([[:- v] & r] :seq)] (recur (drop v doc) doc' r)
      [([[:+ v] & r] :seq)] (recur doc (concat doc' v) r))))

(defn pack
  [op]
  (let [f (fn [op' sop]
            (let [sop' (first op')]
              (match [sop' sop]
                [_       [:= 0]] op'
                [[:= v'] [:= v]] (cons [:= (+ v' v)]   (rest op'))
                [[:+ v'] [:+ v]] (cons [:+ (str v v')] (rest op'))
                [[:- v'] [:- v]] (cons [:- (+ v' v)]   (rest op'))
                :else (cons sop op'))))]
    (reduce f nil (reverse op))))

(defn transform
  [[a b]]
  [a b])
