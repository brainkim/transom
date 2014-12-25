(ns transom.sequential)

(defn ^:private count* [n] (if (number? n) n (count n)))
(defn ^:private take* [n coll] (if (number? coll) (count* n) (take (count* n) coll)))
(defn ^:private drop* [n coll] (if (number? coll) (- coll (count* n)) (drop (count* n) coll)))

(defn count-before
  [edit]
  (letfn
    [(inc-count
      [c [o p]]
      (case o
        :retain (+ c p)
        :delete (+ c p)
        :insert c))]
    (reduce inc-count 0 edit)))

(defn count-after
  [edit]
  (letfn
    [(inc-count
      [c [o p]]
      (case o
        :retain (+ c p)
        :delete c
        :insert (+ c (count p))))]
    (reduce inc-count 0 edit)))

(defn ^:private align-compose
  [edit1 edit2]
  (loop [out [], edit1 edit1, edit2 edit2]
    (cond
      (empty? edit1) (into out (map #(vector nil %) edit2))
      (empty? edit2) (into out (map #(vector % nil) edit1))
      (= :delete (ffirst edit1)) (recur (conj out [(first edit1) nil]) (rest edit1) edit2)
      (= :insert (ffirst edit2)) (recur (conj out [nil (first edit2)]) edit1 (rest edit2))

      :else
      (let [[o1 p1 :as op1] (first edit1)
            [o2 p2 :as op2] (first edit2)]
        (case (compare (count* p1) (count* p2))
          -1
          (recur (conj out [op1 [o2 (take* p1 p2)]])
                 (rest edit1)
                 (cons [o2 (drop* p1 p2)] (rest edit2)))
          0
          (recur (conj out [op1 op2])
                 (rest edit1)
                 (rest edit2))
          1
          (recur (conj out [[o1 (take* p2 p1)] op2])
                 (cons [o1 (drop* p2 p1)] (rest edit1))
                 (rest edit2)))))))

(defn compose-abstract
  ([edit1 edit2]
    (assert (= (count-after edit1) (count-before edit2))
            (str "transom/compose: length mismatch" \newline
                 "Edit 1: " edit1 \newline
                 "Edit 2: " edit2))
    (let [aligned (align-compose edit1 edit2)]
      (reduce
        (fn [out [op1 op2]]
          (let [[o1 p1] op1 [o2 p2] op2]
            (case [o1 o2]
              ;[:insert :insert]
              [:insert :retain] (conj out op1)
              [:insert :delete] out
              ;[:retain :insert]
              [:retain :retain] (conj out op1)
              [:retain :delete] (conj out op2)
              ;[:delete :insert]
              ;[:delete :retain]
              ;[:delete :delete]
              [:delete nil    ] (conj out op1)
              [nil     :insert] (conj out op2))))
        []
        aligned))))

(defn ^:private align-transform
  [edit1 edit2]
  (loop [out [], edit1 edit1, edit2 edit2]
    (cond
      (empty? edit1) (concat out (map #(vector nil %) edit2))
      (empty? edit2) (concat out (map #(vector % nil) edit1))
      (= :insert (ffirst edit1)) (recur (conj out [(first edit1) nil]) (rest edit1) edit2)
      (= :insert (ffirst edit2)) (recur (conj out [nil (first edit2)]) edit1 (rest edit2))

      :else
      (let [[o1 p1 :as op1] (first edit1), [o2 p2 :as op2] (first edit2)]
        (case (compare (count* p1) (count* p2))
          -1
          (recur (conj out [op1 [o2 (take* p1 p2)]])
                 (rest edit1)
                 (cons [o2 (drop* p1 p2)] (rest edit2)))
          0
          (recur (conj out [op1 op2])
                 (rest edit1)
                 (rest edit2))
          1
          (recur (conj out [[o1 (take* p2 p1)] op2])
                 (cons [o1 (drop* p2 p1)] (rest edit1))
                 (rest edit2)))))))

(defn transform-abstract
  [edit1 edit2]
  (assert (= (count-before edit1) (count-before edit2))
          (str "transom/transform: length mismatch" \newline
               "Edit 1: " edit1 \newline
               "Edit 2: " edit2))
  (let [aligned (align-transform edit1 edit2)]
    (reduce
      (fn [out [op1 op2]]
        (let [[o1 p1] op1 [o2 p2] op2]
          (case [o1 o2]
            ;[:insert :insert]
            ;[:insert :retain]
            ;[:insert :delete]
            ;[:retain :insert]
            [:retain :retain] (conj out [op1 op2])
            [:retain :delete] (conj out [nil op2])
            ;[:delete :insert]
            [:delete :retain] (conj out [op1 nil])
            [:delete :delete] out
            [:insert nil    ] (conj out [op1 [:retain (count* p1)]])
            [nil     :insert] (conj out [[:retain (count* p2)] op2]))))
      []
      aligned)))
