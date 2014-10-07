(ns transom.string
  (:require
      #+clj [clojure.core.match :refer [match]]
     #+cljs [cljs.core.match])
  #+cljs
  (:require-macros [cljs.core.match.macros :refer [match]]))

(def ^:private nop? (some-fn nil? #(= (second %) 0) #(= (second %) "")))

(defn pack
  [edit]
  (let [edit (remove nop? edit)]
    (reduce
      (fn [edit op]
        (let [[o1 p1] (peek edit)
              [o2 p2] op]
          (if (= o1 o2)
            (conj (pop edit)
                  (case o1
                    :retain [:retain (+ p1 p2)]
                    :delete [:delete (+ p1 p2)]
                    :insert [:insert (str p1 p2)]))
            (conj edit op))))
      (vec (take 1 edit))
      (rest edit))))

(defn ^:private pack-pairs
  [pairs]
  (map pack (apply mapv vector pairs)))

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

(defn diff
  [a b]
  (if (= a b)
    (pack [[:retain (count a)]]) ;; don't forget to wrap it dummy
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

(defn ^:private align-compose
  [edit1 edit2]
  (loop [edit1 edit1 edit2 edit2 out []]
    (let [op1 (first edit1) op2 (first edit2)]
      (cond
        (nil? op1) (concat out (map #(vector nil %) edit2))
        (nil? op2) (concat out (map #(vector % nil) edit1))
        (= :delete (first op1)) (recur (rest edit1) edit2 (conj out [op1 nil]))
        (= :insert (first op2)) (recur edit1 (rest edit2) (conj out [nil op2]))

        :else
        (let [[o1 p1] op1
              p1' (if (string? p1) (count p1) p1)
              [o2 p2] op2]
          (case (compare p1' p2)
            -1
            (recur (rest edit1) (cons [o2 (- p2 p1')] (rest edit2))
                   (conj out [op1 [o2 p1']]))
            0
            (recur (rest edit1) (rest edit2)
                   (conj out [op1 op2]))
            1
            (let [left (if (string? p1) (apply str (drop p2 p1)) (- p1 p2))
                  taken (if (string? p1) (apply str (take p2 p1)) p2)]
              (recur (cons [o1 left] (rest edit1)) (rest edit2)
                     (conj out [[o1 taken] op2])))))))))

(defn compose
  ([edit1 edit2]
    (assert (= (count-after edit1) (count-before edit2))
            (str "transom/compose: length mismatch" \newline
                 "Edit 1: " edit1 \newline
                 "Edit 2: " edit2))
    (letfn
      [(compare-ops
         [out [op1 op2]]
         (match [op1 op2]
           [[:delete _] _          ] (conj out op1)
           [_           [:insert _]] (conj out op2)
           [[:insert _] [:retain _]] (conj out op1)
           [[:insert _] [:delete _]] out
           [[:retain _] [:retain _]] (conj out op1)
           [[:retain _] [:delete _]] (conj out op2)
           ;; We need these cases for weird edge cases
           ;; e.g. (compose [[:delete 0]] [[:retain 0]])
           [nil _  ] out
           [_   nil] out))]
      (->> (align-compose edit1 edit2)
           (reduce compare-ops [])
           pack)))
  ([edit1 edit2 & more]
    (reduce compose (compose edit1 edit2) more)))

(defn ^:private align-transform
  [edit1 edit2]
  (loop [edit1 edit1, edit2 edit2, out [[] []]]
    (let [op1 (first edit1) op2 (first edit2)]
      (cond
        (nil? op1) (concat out (map #(vector nil %) edit2))
        (nil? op2) (concat out (map #(vector % nil) edit1))
        (= :insert (first op1)) (recur (rest edit1) edit2 (conj out [op1 nil]))
        (= :insert (first op2)) (recur edit1 (rest edit2) (conj out [nil op2]))
        :else
        (let [[o1 p1] op1
              [o2 p2] op2]
          (case (compare p1 p2)
            -1 (recur (rest edit1)
                      (cons [o2 (- p2 p1)] (rest edit2))
                      (conj out [op1 [o2 p1]]))
            0  (recur (rest edit1) (rest edit2)
                      (conj out [op1 op2]))
            1  (recur (cons [o1 (- p1 p2)] (rest edit1))
                      (rest edit2)
                      (conj out [[o1 p2] op2]))))))))

(defn transform
  [edit1 edit2]
  (assert (= (count-before edit1) (count-before edit2))
          (str "transom/transform: length mismatch" \newline
               "Edit 1: " edit1 \newline
               "Edit 2: " edit2))
  (letfn
    [(compare-ops
       [out [op1 op2]]
       (match [op1 op2]
         [[:insert p1] _           ] (conj out [op1 [:retain (count p1)]])
         [_            [:insert p2]] (conj out [[:retain (count p2)] op2])
         [[:retain _ ] [:retain _ ]] (conj out [op1 op2])
         [[:retain p1] [:delete p2]] (conj out [nil op2])
         [[:delete p1] [:retain p2]] (conj out [op1 nil])
         :else (conj out [nil nil])))]
    (->> (align-transform edit1 edit2)
         (reduce compare-ops [])
         pack-pairs)))

(defn transform-caret
  [caret edit]
  ;; caret is the little blinky line thing
  ;; index is where we are in the (new?) document
  (loop [caret caret, index 0, edit edit]
    (if (empty? edit)
      caret
      (let [[o p :as op] (first edit)]
        (case o
          :retain
          (recur caret
                 (+ index p)
                 (rest edit))
          :delete
          (recur (if (> caret index)
                   (max (- caret p) 0)
                   caret)
                 index
                 (rest edit))
          :insert
          (let [op-len (count p)]
            (recur (if (>= caret index)
                     (+ caret op-len)
                     caret)
                   (+ index op-len)
                   (rest edit))))))))
