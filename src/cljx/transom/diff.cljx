(ns transom.diff)

(defn snake
  [^String s1 ^String s2 k y]
  (let [m (count s1)
        n (count s2)
        x (+ k y)]
    (loop [x x, y y]
      (if (and (< x m) (< y n) (= (.charAt s1 x) (.charAt s2 y)))
        (recur (inc x) (inc y))
        y))))

(defn diagonals
  [cost]
  (range (- cost) (inc cost) 2))

(defn furthest
  [s1 s2 k-map cost]
  (reduce
    (fn [k-map k]
      (let [below (k-map (dec k) -1)
            above (k-map (inc k) -1)
            ;; if (= below above) then below is the further k
            y (if (and (> k (- cost)) (> below above))
                below
                (inc above))]
        (assoc k-map k (snake s1 s2 k y))))
    k-map
    (diagonals cost)))

(defn iterate-furthest
  ([s1 s2]
    (iterate-furthest s1 s2 (+ (count s1) (count s2))))
  ([s1 s2 max-cost]
    (let [m (count s1)
          n (count s2)
          sink-k (- m n)
          sink-y n]
      (loop [cost 0, out (list)]
        (if (> cost max-cost)
          nil
          (let [k-map (furthest s1 s2 (or (first out) {}) cost)
                max-y (k-map sink-k)
                out (cons k-map out)]
            (if (= max-y sink-y)
              out
              (recur (inc cost) out))))))))
