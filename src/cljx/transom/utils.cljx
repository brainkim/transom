(ns transom.utils)

(def key-set (comp set keys))

(defn dissocv
  [v i]
  (into (subvec v 0 i) (subvec v (inc i))))
