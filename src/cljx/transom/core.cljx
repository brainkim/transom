(ns transom.core
  (:require [clojure.string :refer [join]] 
            #+clj
            [clojure.core.match :refer [match]]
            #+cljs
            [cljs.core.match])
  #+cljs
  (:require-macros [cljs.core.match.macros :refer [match]]))

(defn- insert
  [s1 s2 [x y path]]
  (if (>= y (count s2))
    nil
    [x (inc y) (conj path [:+ (nth s2 y)])]))

(defn- delete
  [s1 s2 [x y path]]
  (if (>= x (count s1))
    nil
    [(inc x) y (conj path [:- 1])]))

(defn- choose
  [s1 s2 [x1 y1 :as node1] [x2 y2 :as node2]]
  (if (< (+ x1 y1) (+ x2 y2))
    (insert s1 s2 node2)
    (delete s1 s2 node1)))

(defn- wrap
  [node]
  (when-let [node node] [node]))

(defn- snake
  [s1 s2 [x y path]]
  (let [m (count s1), n (count s2)]
    (loop [x x, y y, k 0]
      (if (and (< x m) (< y n) (= (nth s1 x) (nth s2 y)))
        (recur (inc x) (inc y) (inc k))
        (if (> k 0)
          [x y (conj path [:= k])]
          [x y path])))))

(defn- furthest
  [s1 s2 ks]
  (let [f (wrap (insert s1 s2 (first ks)))
        ks' (map (partial apply (partial choose s1 s2))
                 (partition 2 1 ks))
        l (wrap (delete s1 s2 (last ks)))]
    (map (partial snake s1 s2) (concat f ks' l))))

(def ^:private initial-node [0 0 []])

(defn diff
  [s1 s2]
  (let [m (count s1)
        n (count s2)
        furthest* (partial furthest s1 s2)
        initial [(snake s1 s2 initial-node)]
        iteration (take-while (comp not empty?) (iterate furthest* initial))]
    (loop [iter iteration]
      (when-let [current (first iter)]
        (if-let [sink (first (filter (fn [[x y]] (and (= x m) (= y n))) current))]
          (nth sink 2)
          (recur (rest iter)))))))

(defn count-op
  [[o p]]
  (case o
    := p
    :- p
    :+ (count p)))

(defn count-before
  [edit]
  (reduce
    (fn [c [o p]]
      (case o
        := (+ c p)
        :- (+ c p)
        :+ c))
    0 edit))

(defn count-after
  [edit]
  (reduce
    (fn [c [o p]]
      (case o
        := (+ c p)
        :- c
        :+ (+ c (count p))))
    0 edit))

(defn- patch*
  [doc edit]
  (assert (= (count doc) (count-before edit))
          (str "patch: The length of the document (" (count doc) ") does not "
               "match the length of the edit (" (count-before edit) ")."))
  (loop [doc doc, doc' nil, edit edit]
    (match [edit]
      [([] :seq)] (apply str doc')
      [([[:= p] & r] :seq)] (recur (drop p doc) (concat doc' (take p doc)) r)
      [([[:- p] & r] :seq)] (recur (drop p doc) doc' r)
      [([[:+ p] & r] :seq)] (recur doc (concat doc' p) r))))

(defn patch
  [doc & edits]
  (reduce patch* doc edits))

(defn reverse-pack
  [edit]
  (reduce
    (fn [edit' op]
      (let [op' (first edit')]
        (match [op' op]
          [_ [_ 0]] edit'
          [_ :nop] edit'
          [[:= p'] [:= p]] (cons [:= (+ p p')] (rest edit'))
          [[:- p'] [:- p]] (cons [:- (+ p p')] (rest edit'))
          [[:+ p'] [:+ p]] (cons [:+ (str p p')] (rest edit'))
          :else (cons op edit'))))
    [] edit))

(defn pack
  [edit]
  (reverse-pack (reverse edit)))

(defn pack-pairs
  [pairs]
  (map pack (apply mapv vector pairs)))

(defn align-transform
  [edit1 edit2]
  (loop [edit1 edit1 edit2 edit2 out []]
    (let [op1 (first edit1) op2 (first edit2)]
      (cond
        (nil? op1) (concat out (map #(vector :nop %) edit2))

        (nil? op2) (concat out (map #(vector % :nop) edit1))

        (= :+ (first op1)) (recur (rest edit1) edit2 (conj out [op1 :nop]))

        (= :+ (first op2)) (recur edit1 (rest edit2) (conj out [:nop op2]))

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
          (str "The length of the two edits (" (count-before edit1) ", "
               (count-before edit2) ") do not match."))
  (pack-pairs
    (reduce (fn [out [op1 op2]]
              (match [op1 op2]
                [[:+ p] _]
                (conj out [op1 [:= (count p)]])
                [_ [:+ p]]
                (conj out [[:= (count p)] op2])
                [[:= _] [:= _]]
                (conj out [op1 op1]) ;; (= sop1 sop2)
                [[:= p1] [:- p2]]
                (conj out [:nop op2])
                [[:- p1] [:= p2]]
                (conj out [op1 :nop])
                :else out))
            [] (align-transform edit1 edit2))))

(defn align-compose
  [edit1 edit2]
  (loop [edit1 edit1 edit2 edit2 out []]
    (let [op1 (first edit1) op2 (first edit2)]
      (cond
        (nil? op1) (concat out (map #(vector :nop %) edit2))

        (nil? op2) (concat out (map #(vector % :nop) edit1))

        (= :- (first op1)) (recur (rest edit1) edit2 (conj out [op1 :nop]))

        (= :+ (first op2)) (recur edit1 (rest edit2) (conj out [:nop op2]))

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
  [edit1 edit2]
  (pack
    (reduce (fn [out [op1 op2]]
              (match [op1 op2]
                [[:- _] _]
                (conj out op1)
                [_ [:+ _]]
                (conj out op2)
                [[:+ _] [:= _]]
                (conj out op1)
                [[:+ _] [:- _]]
                out
                [[:= _] [:= _]]
                (conj out op1) ;; (= op1 op2)
                [[:= _] [:- _]]
                (conj out op2)))
            [] (align-compose edit1 edit2))))

(defn transform-caret
  [caret edit]
  ;; caret is the little blinky line thing
  ;; index is where we are in the document
  (assert (<= caret (count-before edit)))
  (loop [caret caret, index 0, edit edit]
    (if (<= caret index) ;; < ?
      caret
      (let [op (first edit)
            caret (case (first op)
                    := caret
                    :- (- caret (second op))
                    :+ (+ caret (count (second op))))
            index (+ index (count-op op))]
        (recur caret index (rest edit))))))