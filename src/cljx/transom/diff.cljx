(ns transom.diff)
(comment
;                                    s1 s2
;     add a letter from s2 and inc y <- -> delete a letter from s1 and inc x
                                  "abc" "xyz"
                                    [a0 x0]
                                [a0 y1] [b1 x0]
                            [a0 z2] [b1 y1] [c2 x0]
                        [a0 :3] [b1 z2] [c2 y1] [:3 x0]
                    [a0 :4] [b1 :3] [c2 z2] [:3 y1] [:4 x0]
                [a0 :5] [b1 :4] [c2 :3] [:3 z2] [:4 y1] [:5 x0]
            [a0 :6] [b1 :5] [c2 :4] [:3 :3] [:4 z2] [:5 y1] [:6 x0]

[:+ x y z] [:- a b c]

                                "snake" "snail"
                                    [s0 s0]
                                    [n1 n1]
                                    [a2 a2]
                                [a2 i3] [k3 a2]
                            [a2 l4] [k3 i3] [e4 a2]
                        [a2 :5] [k3 l4] [e4 i3] [:5 a2]
                    [a2 :6] [k3 :5] [e4 l4] [:5 i3] [:6 a2]
                [a2 :7] [k3 :6] [e4 :5] [:5 l4] [:6 i3] [:7 a2]
            [a2 :8] [k3 :7] [e4 :6] [:5 :5] [:6 i3] [:7 i3] [:8 a2]

[:= s n a] [:+ i l] [:- k e]

                               "foobar" "phobar"
                                    [f0 p0]
                                [f0 h1] [o1 p0]
                            [f0 o2] [o1 h1] [o2 p0]
                        [f0 b3] [o1 o2] [o2 h1] [b3 p0]
                                [o2 b3]
                    [f0 a4] [o2 a4] [b3 b3] [b3 h1] [a4 p0]
                                    [a4 a4]
                                    [r5 r5]
                                    [:6 :6]

[:+ p h] [:- f] [:= o] [:- o] [:= b a r]

                                "pizza" "pasta"
                                    [p0 p0]
                                    [i1 a1]
                                [i1 s2] [z2 a1]
                            [i1 t3] [z2 s2] [z3 a1]
                        [i1 a4] [z2 t3] [z3 s2] [a4 a1]
                                                [:5 s2]
                    [i1 :5] [z2 a4] [z3 t3] [:5 t3] [:6 s2]
                [i1 :6] [z2 :6] [z3 a4] [a4 t3] [:6 t3] [:7 s2]
            [i1 :7] [z2 :7] [z3 :7] [a4 a4] [:6 a4] [:7 t3] [:8 s2]
                                    [:5 :5] [:6 :5]

[:= p] [:+ a s t] [:- i z z] [:= a] ?

                                "pizza" "piazza"
                                    [p0 p0]
                                    [i1 i1]
                                    [z2 a2]
                                [z2 z3] [z3 a2]
                                [z3 z4]
                                [a4 a5]
                                [:5 :6]

[:= p i] [:+ a] [:= z z a]

                             "momofuku" "mamma"
                                    [m0 m0]
                                    [o1 a1]
                                [o1 m2] [m2 a1]
                            [o1 m3] [m2 m2] [o3 a1]
                                    [o3 m3]
                        [o1 a4] [o3 a4] [f4 m3] [f4 a1]
                    [o1 :5] [o3 :5] [f4 a4] [u5 m3] [u5 a1]
                [o1 :6] [o3 :6] [f4 :5] [u5 a4] [k6 m3] [k6 a1]
            [o1 :7] [o3 :7] [f4 :6] [u5 :5] [k6 a4] [u7 m3] [u7 a1]
        [o1 :8] [o3 :8] [f4 :7] [u5 :6] [k6 :5] [u7 a4] [:8 m3] [:8 a1]
    [o1 :9] [o3 :9] [f4 :8] [u5 :7] [k6 :6] [u7 :5] [:8 a4] [:9 m3] [:9 a1]
[o1 10] [o3 10] [f4 :9] [u5 :8] [k6 :7] [u7 :6] [:8 :5] [:9 a4] [10 m3] [10 a1]

[:= m] [:+ a] [:- o] [:= m] [:+ m a] [:- o f u k u] ?

;; We go right if left is larger, left is right is larger
;; In the case of a tie, choose left?
)

(defn snake
  [s1 s2 [x y path]]
  (let [m (count s1), n (count s2)]
    (loop [x x, y y, k 0]
      (if (and (< x m) (< y n) (= (nth s1 x) (nth s2 y)))
        (recur (inc x) (inc y) (inc k))
        (if (> k 0)
          [x y (conj path [:= k])]    
          [x y path])))))

(defn choose
  [s1 s2 [x1 y1 path1] [x2 y2 path2]]
  (if (> (+ x1 y1) (+ x2 y2))
    (snake s1 s2 [(inc x1) y1 (conj path1 [:- 1])])
    (snake s1 s2 [x2 (inc y2) (conj path2 [:+ (nth s2 y2 nil)])])))

(defn furthest
  [s1 s2 ks]
  (let [[x-f y-f path-f] (first ks)
        f [(snake s1 s2 [x-f (inc y-f) (conj path-f [:+ (nth s2 y-f nil)])])]
        ks' (map (partial apply (partial choose s1 s2)) (partition 2 1 ks))
        [x-l y-l path-l] (last ks)
        l [(snake s1 s2 [(inc x-l) y-l (conj path-l [:- 1])])]]
    (concat f ks' l)))

(defn diff
  [s1 s2]
  (let [m (count s1)
        n (count s2)
        furthest* (partial furthest s1 s2)
        initial [(snake s1 s2 [0 0 []])]
        iteration (iterate furthest* initial)]
    (loop [iter iteration]
      (let [current (first iter)] 
        (if-let [sink (first (filter
                               (fn [[x y]] (and (= x m) (= y n)))
                               current))]
          (nth sink 2)
          (recur (rest iter)))))))
