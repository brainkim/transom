(ns transom.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [join]]))

(defn count-before
  [op]
  (reduce
    (fn [c sop]
      (match [sop]
        [[:= v]] (+ c v)
        [[:- v]] (+ c v)
        [[:+ v]] c))
    0 op))

(defn count-after
  [op]
  (reduce 
    (fn [c sop]
      (match [sop]
        [[:= v]] (+ c v)
        [[:- v]] c
        [[:+ v]] (+ c (count v))))
    0 op))

(defn apply-op
  [doc op]
    (assert (= (count doc) (count-before op))
            (str "apply-op: The length of the document " (count doc)
                 " does not match the before-length of the operation " 
                 (count-before op) "."))
    (loop [doc doc, doc' nil, op op]
      (match [op]
        [([] :seq)] (apply str doc')
        [([[:= v] & r] :seq)] (recur (drop v doc) (concat doc' (take v doc)) r)
        [([[:- v] & r] :seq)] (recur (drop v doc) doc' r)
        [([[:+ v] & r] :seq)] (recur doc (concat doc' v) r))))

(defn apply-ops
  [doc & ops]
  (reduce apply-op doc ops))

(defn reverse-pack
  [op]
  (reduce
    (fn [op' sop]
      (let [sop' (first op')]
        (match [sop' sop]
          [_       [:= 0]] op'
          [_       :nop  ] op'
          [[:= v'] [:= v]] (cons [:= (+ v' v)]   (rest op'))
          [[:- v'] [:- v]] (cons [:- (+ v' v)]   (rest op'))
          [[:+ v'] [:+ v]] (cons [:+ (str v v')] (rest op'))
          :else (cons sop op'))))
    nil op))

(defn pack
  [op]
  (reverse-pack (reverse op)))

(defn pack-pairs
  [pairs]
  (map pack (apply mapv vector pairs)))

(defn consumes?
  [[o p]]
  (contains? #{:= :-} o))

(defn align
  [op1 op2]
  (loop [op1 op1 op2 op2 out []]
    (let [sop1 (first op1) sop2 (first op2)]
      (cond
        (nil? sop1)
        (concat out (map #(vector :nop %) op2))

        (nil? sop2)
        (concat out (map #(vector % :nop) op1))

        (not (consumes? sop1))
        (recur (rest op1) op2 (conj out [sop1 :nop]))

        (not (consumes? sop2))
        (recur op1 (rest op2) (conj out [:nop sop2]))

        :else
        (let [[o1 p1] sop1
              [o2 p2] sop2]
          (condp = (compare p1 p2)
            -1 (recur (rest op1) (cons [o2 (- p2 p1)] (rest op2))
                      (conj out [sop1 [o2 p1]]))
            0  (recur (rest op1) (rest op2)
                      (conj out [sop1 sop2]))
            1  (recur (cons [o1 (- p1 p2)] (rest op1)) (rest op2)
                      (conj out [[o1 p2] sop2]))))))))


(defn transform
  [op1 op2]
  (assert (= (count-before op1) (count-before op2))
          (str "The length of the two edits (" (count-before op1) ", "
               (count-before op2) ") do not match."))
  (pack-pairs
    (reduce (fn [out [sop1 sop2]]
              (match [sop1 sop2]
                [[:+ p] _]
                (conj out [sop1 [:= (count p)]])
                [_ [:+ p]]
                (conj out [[:= (count p)] sop2])
                [[:= _] [:= _]]
                (conj out [sop1 sop1]) ;; (= sop1 sop2)
                [[:= p1] [:- p2]]
                (conj out [:nop sop2])
                [[:- p1] [:= p2]]
                (conj out [sop1 :nop])
                :else out))
            [] (align op1 op2))))

(defn align-compose
  [op1 op2]
  (loop [op1 op1 op2 op2 out []]
    (let [sop1 (first op1) sop2 (first op2)]
      (cond
        (nil? sop1)
        (concat out (map #(vector :nop %) op2))

        (nil? sop2)
        (concat out (map #(vector % :nop) op1))

        (= :- (first sop1))
        (recur (rest op1) op2 (conj out [sop1 :nop]))

        (= :+ (first sop2))
        (recur op1 (rest op2) (conj out [:nop sop2]))

        :else
        (let [[o1 p1] sop1
              p1' (if (string? p1) (count p1) p1)
              [o2 p2] sop2]
          (condp = (compare p1' p2)
            -1
            (recur (rest op1) (cons [o2 (- p2 p1')] (rest op2))
                   (conj out [sop1 [o2 p1']]))
            0
            (recur (rest op1) (rest op2) (conj out [sop1 sop2]))
            1
            (let [left  (if (string? p1)
                          (join (drop p2 p1))
                          (- p1 p2))
                  taken (if (string? p1)
                          (join (take p2 p1))
                          p2)]
              (recur (cons [o1 left] (rest op1)) (rest op2)
                     (conj out [[o1 taken] sop2])))))))))

(comment 
(defn compose
  [op1 op2]
  (assert (= (count-after op1) (count-before op2)))
  (loop [op1 op1 op2 op2 out []]
    (let [sop1 (first op1) sop2 (first op2)]
      (cond (nil? sop1) (concat out op2)
            (nil? sop2) (concat out op1)
            :else 
            (match [sop1 sop2]
        [[:+ p1] [:+ p2]]
        (recur (rest op1) (rest op2) (concat out [sop2 sop1]))
        [[:- _] _]
        (recur (rest op1) op2 (conj out sop1))
        [[:+ p1] [:= p2]]
        (condp = (compare (count p1) p2)
          -1
          (recur (rest op1) (cons [:= (- p2 (count p1))] (rest op2))
                 (conj out sop1))
          0
          (recur (rest op1) (rest op2)
                 (conj out sop1))
          1
          (recur (cons [:+ (drop p2 p1)] (rest op1)) (rest op2)
                 (conj out [:+ (take p2 p1)])))
        [[:+ p1] [:- p2]]
        (condp = (compare (count p1) p2)
          -1
          (recur (rest op1) (cons [:- (- p2 (count p1))] (rest op2))
                 out)
          0
          (recur (rest op1) (rest op2) out)
          1
          (recur (cons [:+ (drop p2 p1)] (rest op1)) (rest op2)
                 out))
        [[:= p1] [:+ p2]]
        (condp = (compare p1 (count p2))
          -1
          (recur (rest op1) (rest op2) (conj out sop2))
          0
          (recur (rest op1) (rest op2) (conj out sop2))
          1
          (recur (cons [:= (- p1 (count p2))] (rest op1))
                 (rest op2) (conj out sop2)))
        [[:= p1] [:= p2]]
        (condp = (compare p1 p2)
          -1
          (recur (rest op1) (cons [:= (- p2 p1)] (rest op2)) (conj out sop1))
          0
          (recur (rest op1) (rest op2) (conj out sop1))
          1
          (recur (cons [:= (- p1 p2)] (rest op1)) (rest op2) (conj out sop2)))
        [[:= p1] [:- p2]]
        (condp = (compare p1 p2)
          -1
          (recur (rest op1) (cons [:- (- p2 p1)] (rest op2))
                 (conj out [:- p1]))
          0
          (recur (rest op1) (rest op2) (conj out sop2))
          1
          (recur (cons [:= (- p1 p2)] (rest op1)) (rest op2)
                 (conj out sop2)))))))))
