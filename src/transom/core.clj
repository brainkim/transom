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
    (loop [doc doc, doc' nil, op op]
      (match [op]
        [([] :seq)] (apply str doc')
        [([[:= v] & r] :seq)] (recur (drop v doc) (concat doc' (take v doc)) r)
        [([[:- v] & r] :seq)] (recur (drop v doc) doc' r)
        [([[:+ v] & r] :seq)] (recur doc (concat doc' v) r))))

(defn apply-ops
  [doc & ops]
  (reduce apply-op doc ops)) 

(defn pack
  [op]
  (let [f (fn [op' sop]
            (let [sop' (first op')]
              (match [sop' sop]
                [_       [:= 0]] op'
                [[:= v'] [:= v]] (cons [:= (+ v' v)]   (rest op'))
                [[:- v'] [:- v]] (cons [:- (+ v' v)]   (rest op'))
                [[:+ v'] [:+ v]] (cons [:+ (str v v')] (rest op'))
                :else (cons sop op'))))]
    (reduce f nil (reverse op))))

(defn transform
  [[op1 op2]]
  (assert (= (count-op op1) (count-op op2)))
  (loop [[op1 op2] [op1 op2], [op1' op2'] [nil nil]]
    (let [sop1 (first op1) sop2 (first op2)]
      (match [sop1 sop2]
        [nil nil] [(reverse op1') (reverse op2')]
        [[:+ _] _]
          (recur [(rest op1) op2] [op1' (cons sop1 op2')])
        [_ [:+ v1]]
          (recur [op1 (rest op2)] [(cons sop2 op1') op2'])
        [[:- v1] [:- v2]]
          (condp = (compare v1 v2)
            -1 (recur [(rest op1) (cons [:- (- v2 v1)] (rest op2))]
                      [op1' op2'])
            0  (recur [(rest op1) (rest op2)]
                      [op1' op2'])
            1  (recur [(cons [:- (- v1 v2)] (rest op1)) (rest op2)]
                      [op1' op2']))
        [[:= v1] [:= v2]]
          (cond
            (= v1 v2)
            (recur [(rest op1) (rest op2)]
                   [(cons sop1 op1') (cons sop2 op2')])
            (> v1 v2)
              (recur [(cons [:= (- v1 v2)] (rest op1)) (rest op2)]
                     [(cons sop2 op1') (cons sop2 op2')])
            (< v1 v2)
              (recur [(rest op1) (cons [:= (- v2 v1)] (rest op2))]
                     [(cons sop1 op1') (cons sop1 op2')]))
        [[:= v1] [:- v2]]
          (cond
            (= v1 v2)
            (recur [(rest op1) (rest op2)]
                   [op1' (cons sop2 op2')])
            (> v1 v2)
            (recur [(cons [:= (- v1 v2)] (rest op1)) (rest op2)]
                   [op1' (cons sop2 op2')])
            (< v1 v2)
            (recur [(rest op1) (cons [:- (- v2 v1)] (rest op2))]
                   [op1' (cons sop2 op2')]))
        [[:- v1] [:= v2]]
          (cond
            (= v1 v2)
            (recur [(rest op1) (rest op2)]
                   [(cons sop1 op1') op2'])
            (> v1 v2)
            (recur [(rest op1) (rest op2)]
                   [(cons sop1 op1') op2'])
            (< v1 v2)
            (recur [(rest op1) (cons [:= (- v2 v1)] rest op2)]
                   [(cons sop1 op1') op2']))))))
