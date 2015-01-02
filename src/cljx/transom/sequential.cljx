(ns transom.sequential)

(defn ^:private count* [a] (if (number? a) a (count a)))

(def ^:private nop? (some-fn nil? #(= 0 (count* (second %)))))

(defn ^:private pack-abstract
  [edit]
  (if (empty? edit)
    []
    (reduce
      (fn [edit op]
        (let [[o1 p1] (peek edit), [o2 p2] op]
          (if (= o1 o2)
            (conj (pop edit)
                  (case o1
                    :retain [o1 (+ p1 p2)]
                    :delete [o1 (+ p1 p2)]
                    :insert [o1 (concat p1 p2)]))
            (conj edit op))))
      (vector (first edit))
      (rest edit))))

(defn pack-with
  [pack-fn]
  (fn [edit]
    (->> edit
         (remove nop?)
         pack-abstract
         (map (fn [[o p :as op]] (if (= o :insert) [o (pack-fn p)] op))))))

(defn ^:private prefix-length 
  [a b]
  (let [a* (count a)
        b* (count b)]
    (loop [i 0]
      (if (and (< i a*) (< i b*) (= (nth a i) (nth b i)))
        (recur (inc i))
        i))))

(defn ^:private suffix-length 
  [a b]
  (let [a* (count a)
        b* (count b)]
    (loop [i 0]
      (if (and (< i a*) (< i b*)
               (= (nth a (- a* i 1)) (nth b (- b* i 1))))
        (recur (inc i))
        i))))

(defn ^:private create-edit
  [a b p s]
  [[:retain p]
   [:delete (- (count a) p s)]
   [:insert (take (- (count b) p s) (drop p b))]
   [:retain s]])

(defn diff-abstract
  [a b]
  (if (= a b)
    [[:retain (count a)]] ; don't forget to wrap it dummy
    (let [a (vec a), b (vec b)
          p (prefix-length a b), s (suffix-length a b)]
      (if (< p s)
        (let [a' (subvec a 0 (- (count a) s))
              b' (subvec b 0 (- (count b) s))
              p (prefix-length a' b')]
          (create-edit a b p s))
        (let [a' (subvec a p)
              b' (subvec b p)
              s (suffix-length a' b')]
          (create-edit a b p s))))))

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

(defn patch-abstract
  [doc edit]
  (assert (= (count doc) (count-before edit))
          (str "transom/patch: length mismatch" \newline
               "Doc: " doc \newline
               "Edit: " edit))
  (second (reduce
            (fn [[doc out] [o p]]
              (case o
                :retain [(drop p doc) (concat out (take p doc))]
                :delete [(drop p doc) out]
                :insert [doc (concat out p)]))
            [doc '()]
            edit)))

(defn ^:private take*
  [n coll]
  (if (number? coll)
    (count* n)
    (take (count* n) coll)))

(defn ^:private drop*
  [n coll]
  (if (number? coll)
    (- coll (count* n))
    (drop (count* n) coll)))

(defn ^:private align-compose
  [edit1 edit2]
  (loop [out [], edit1 (seq edit1), edit2 (seq edit2)]
    (cond
      (empty? edit1) (into out (map #(vector nil %) edit2))
      (empty? edit2) (into out (map #(vector % nil) edit1))
      (= :delete (ffirst edit1)) (recur (conj out [(first edit1) nil]) (rest edit1) edit2)
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
  (let [aligned (align-transform edit1 edit2)
        pairs
        (reduce
          (fn [pairs [op1 op2]]
            (let [[o1 p1] op1 [o2 p2] op2]
              (case [o1 o2]
                ;[:insert :insert]
                ;[:insert :retain]
                ;[:insert :delete]
                ;[:retain :insert]
                [:retain :retain] (conj pairs [op1 op2])
                [:retain :delete] (conj pairs [nil op2])
                ;[:delete :insert]
                [:delete :retain] (conj pairs [op1 nil])
                [:delete :delete] pairs
                [:insert nil    ] (conj pairs [op1 [:retain (count* p1)]])
                [nil     :insert] (conj pairs [[:retain (count* p2)] op2]))))
          []
          aligned)]
    (if (empty? pairs)
      pairs
      (apply map vector pairs))))
