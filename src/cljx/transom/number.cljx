(ns transom.number)

(defn patch
  ([n [o p]]
    (+ n p))
  ([n edit & more]
    (reduce patch (patch n edit) more)))

(defn diff
  [n1 n2]
  [:+ (- n2 n1)])

(defn transform
  [edit1 edit2]
  [edit1 edit2])

(defn compose
  ([[_ p1] [_ p2]]
    [:+ (+ p1 p2)])
  ([edit1 edit2 & more]
    (reduce compose (compose edit1 edit2) more)))
