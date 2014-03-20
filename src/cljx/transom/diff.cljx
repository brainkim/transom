(ns transom.diff)

(defn snake
  [^String s1 ^String s2 x k]
  (let [m (count s1)
        n (count s2)
        y (- x k)]
    (loop [x x, y y]
      (if (and (< x m) (< y n) (= (.charAt s1 x) (.charAt s2 y)))
        (recur (inc x) (inc y))
        x))))

(defn diagonals
  [cost]
  (range (- cost) (inc cost) 2))

(defn furthest
  [s1 s2 prev cost]
  (reduce
    (fn [endpoints k]
      (let [below (get endpoints (dec k) -1)
            above (get endpoints (inc k) -1)
            x (if (and (< k cost) (< below above))
                above
                (inc below))]
        (assoc endpoints k (snake s1 s2 x k))))
    prev
    (diagonals cost)))

(defn iterate-search
  [s1 s2]
  (let [m (count s1)
        n (count s2)
        goal-x m
        goal-k (- m n)]
    (loop [cost 0
           out (list)]
      (let [f-map (furthest s1 s2 (or (first out) {}) cost)
            out (cons f-map out)
            [max-k max-x] (apply max-key (fn [[k v]] (+ v k)) f-map)]
        (if (and (>= max-k goal-k) (= max-x goal-x))
          out
          (recur (inc cost) out))))))

(defn ->edit
  [f-maps]
  (reduce 
    (fn [edit f-map]
      (list))
    (list)
    f-maps))

(defn diff
  [s1 s2]
  (->edit (iterate-search s1 s2)))
