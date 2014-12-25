(ns transom.string)

(def ^:private nop? (some-fn nil? #(= (second %) 0) #(= (second %) "")))

(defn pack
  [edit]
  (let [edit (remove nop? edit)
        edit (map (fn [[o p :as op]] (if (= o :insert) [o (apply str p)] op)) edit)]
    (if (empty? edit)
      []
      (reduce
        (fn [edit op]
          (let [[o1 p1] (peek edit), [o2 p2] op]
            (if (= o1 o2)
              (conj (pop edit)
                    (case o1 :retain [:retain (+ p1 p2)]
                             :delete [:delete (+ p1 p2)]
                             :insert [:insert (str p1 p2)]))
              (conj edit op))))
        (vector (first edit))
        (rest edit)))))

(defn ^:private pack-pairs
  [pairs]
  (if (empty? pairs)
    pairs
    (map pack (apply map vector pairs))))

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
  (pack [[:retain pre]
         [:delete (- (count a) pre suf)]
         [:insert (subs b pre (- (count b) suf))]
         [:retain suf]]))

;; NOTE(brian): one thing I don't like about this edit representation is that a
;;   no-op is represented as an edit with a single :retain op. Having to keep
;;   track of :retains makes edits poorly formed.
;;   e.g. the empty edit could just as well be represented as [] or [[:retain 0]]
(defn diff
  [a b]
  (if (= a b)
    (pack [[:retain (count a)]]) ; don't forget to wrap it dummy
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

(defn patch
  ([doc edit]
    (assert (= (count doc) (count-before edit))
            (str "transom/patch: length mismatch" \newline
                 "Doc: " doc \newline
                 "Edit: " edit))
    (second (reduce
              (fn [[doc out] [o p]]
                (case o
                  :retain [(subs doc p) (str out (subs doc 0 p))]
                  :delete [(subs doc p) out]
                  :insert [doc (str out p)]))
              [doc ""]
              edit)))
  ([doc edit & edits]
    (reduce patch (patch doc edit) edits)))

(defn ^:private count* [n] (if (number? n) n (count n)))
(defn ^:private take* [n coll] (if (number? coll) (count* n) (take (count* n) coll)))
(defn ^:private drop* [n coll] (if (number? coll) (- coll (count* n)) (drop (count* n) coll)))

(defn ^:private align-compose
  [edit1 edit2]
  (loop [out [], edit1 edit1, edit2 edit2]
    (cond
      (empty? edit1) (concat out (map #(vector nil %) edit2))
      (empty? edit2) (concat out (map #(vector % nil) edit1))
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

(defn compose
  ([edit1 edit2]
    (assert (= (count-after edit1) (count-before edit2))
            (str "transom/compose: length mismatch" \newline
                 "Edit 1: " edit1 \newline
                 "Edit 2: " edit2))
    (let [aligned (align-compose edit1 edit2)]
      (pack
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
  ([edit1 edit2 & more]
    (reduce compose (compose edit1 edit2) more)))

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

(defn transform
  [edit1 edit2]
  (assert (= (count-before edit1) (count-before edit2))
          (str "transom/transform: length mismatch" \newline
               "Edit 1: " edit1 \newline
               "Edit 2: " edit2))
  (let [aligned (align-transform edit1 edit2)]
    (pack-pairs
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
        aligned))))

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
