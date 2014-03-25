(ns transom.diff)

(defn- insert
  [s1 s2 [x y path]]
  (if (>= y (count s2))
    nil
    [[x (inc y) (conj path [:+ (nth s2 y)])]]))

(defn- delete
  [s1 s2 [x y path]]
  (if (>= x (count s1))
    nil
    [[(inc x) y (conj path [:- 1])]]))

(defn- snake
  [s1 s2 [x y path]]
  (let [m (count s1), n (count s2)]
    (loop [x x, y y, k 0]
      (if (and (< x m) (< y n) (= (nth s1 x) (nth s2 y)))
        (recur (inc x) (inc y) (inc k))
        (if (> k 0)
          [x y (conj path [:= k])]
          [x y path])))))

(defn- choose
  [s1 s2 [x1 y1 path1] [x2 y2 path2]]
  (if (< (+ x1 y1) (+ x2 y2))
    [x2 (inc y2) (conj path2 [:+ (nth s2 y2)])]
    [(inc x1) y1 (conj path1 [:- 1])]))

(defn furthest
  [s1 s2 ks]
  (let [f (insert s1 s2 (first ks))
        ks' (map (partial apply (partial choose s1 s2)) (partition 2 1 ks))
        l (delete s1 s2 (last ks))]
    (concat f ks' l)))

(defn diff
  [s1 s2]
  (let [m (count s1)
        n (count s2)
        furthest* (partial furthest s1 s2)
        initial [(snake s1 s2 [0 0 []])]
        iteration (iterate furthest* initial)]
    (loop [iter iteration]
      (when-let [current (first iter)] 
        (if-let [sink (first (filter (fn [[x y]] (and (= x m) (= y n)))
                                     current))]
          (nth sink 2)
          (recur (rest iter)))))))
