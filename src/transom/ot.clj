(ns transom.ot
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [join]]))

(defn count-sop
  [[o p]]
  (match [o]
    [:=] p
    [:-] p 
    [:+] (count p)))

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

(defn align-transform
  [op1 op2]
  (loop [op1 op1 op2 op2 out []]
    (let [sop1 (first op1) sop2 (first op2)]
      (cond
        (nil? sop1)
        (concat out (map #(vector :nop %) op2))

        (nil? sop2)
        (concat out (map #(vector % :nop) op1))

        (= :+ (first sop1))
        (recur (rest op1) op2 (conj out [sop1 :nop]))

        (= :+ (first sop2))
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
            [] (align-transform op1 op2))))

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

(defn compose
  [op1 op2]
  (pack
    (reduce (fn [out [sop1 sop2]]
              (match [sop1 sop2]
                [[:- _] _]
                (conj out sop1)
                [_ [:+ _]]
                (conj out sop2)
                [[:+ _] [:= _]]
                (conj out sop1)
                [[:+ _] [:- _]]
                out
                [[:= _] [:= _]]
                (conj out sop1) ;; (= sop1 sop2)
                [[:= _] [:- _]]
                (conj out sop2)))
            [] (align-compose op1 op2))))

(defn transform-caret
  [caret op]
  ;; caret is the little blinky line thing
  ;; idx is where we are in the document
  (assert (<= caret (count-before op)))
  (loop [caret caret idx 0 op op]
    (if (< caret idx) ;; (<= ?)
      caret
      (let [sop (first op)
            caret' (cond
                     (= := (first sop)) caret
                     (= :- (first sop)) (- caret (second sop))
                     (= :+ (first sop)) (+ caret (count (second sop))))]
        (recur caret' (+ idx (count-sop sop)) (rest op))))))
