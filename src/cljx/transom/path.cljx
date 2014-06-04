(ns transom.path)

(defn prefix?
  [path-1 path-2]
  (and (< (count path-1) (count path-2))
       (every? (partial apply =) (map vector path-1 path-2))))

(defn suffix
  [path-1 path-2]
  (when (prefix? path-1 path-2)
    (nthnext path-2 (count path-1))))

(defn generality
  [[path _]]
  (count path))

(defn specificity
  [[path _]]
  (- (count path)))
