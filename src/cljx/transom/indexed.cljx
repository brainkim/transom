(ns transom.indexed
  (:require
      #+clj [clojure.core.match :refer [match]]
     #+cljs [cljs.core.match])
  #+cljs
  (:require-macros [cljs.core.match.macros :refer [match]]))

(defn ^:private count-before
  [{:keys [edit]}]
  (letfn
    [(inc-count
      [c [o p]]
      (case o
        :retain (+ c p)
        :delete (+ c p)
        :insert c))]
    (reduce inc-count 0 edit)))

(defn ^:private count-after
  [{:keys [edit]}]
  (letfn
    [(inc-count
      [c [o p]]
      (case o
        :retain (+ c p)
        :delete c
        :insert (+ c (count p))))]
    (reduce inc-count 0 edit)))

(defn patch
  ([doc {:keys [edit]}]
    (assert (= (count doc) (count-before edit))
            (str "transom/patch: length mismatch" \newline
                 "Doc: " doc \newline
                 "Edit: " edit))
    (loop [doc doc, edit edit, out nil]
      (if (empty? edit)
        out
        (let [[o p] (first edit)
              r (rest edit)]
          (case o
            :retain (recur (drop p doc) r (concat out (take p doc)))
            :delete (recur (drop p doc) r out)
            :insert (recur doc r (concat out p)))))))
  ([doc operation & operations]
    (reduce patch (patch doc operation) operations)))

(defn pack
  [{:keys [edit]}]
  (letfn
    [(nop?
      [op]
      (or (keyword? op) (= (second op) 0) (= (second op) [])))
     (combinable?
       [[o1] [o2]]
       (= o1 o2))
     (combine
       [op1 op2]
       (match [op1 op2]
         [[:retain p1] [:retain p2]] [:retain (+ p1 p2)]
         [[:delete p1] [:delete p2]] [:delete (+ p1 p2)]
         [[:insert p1] [:insert p2]] [:insert (concat p1 p2)]))
     (reducer
       ;; pop as in previous op
       ;; cop as in current op
       [[pop edit' :as prev] cop]
       (cond
         (nop? cop) prev
         (nil? pop) [cop edit']
         (combinable? pop cop) [(combine pop cop) edit']
         :else [cop (conj edit' pop)]))]
    (let [[pop edit'] (reduce reducer [nil []] edit)]
      (if (nil? pop)
        {:edit edit'}
        {:edit (conj edit' pop)}))))

(defn pack-pairs
  [pairs]
  (map pack (apply mapv vector pairs)))

(defn ^:private align-transform
  [edit1 edit2]
  (loop [edit1 edit1, edit2 edit2, out []]
    (let [op1 (first edit1) op2 (first edit2)]
      (cond
        (nil? op1) (concat out (map #(vector :nop %) edit2))
        (nil? op2) (concat out (map #(vector % :nop) edit1))
        (= :insert (first op1)) (recur (rest edit1) edit2 (conj out [op1 :nop]))
        (= :insert (first op2)) (recur edit1 (rest edit2) (conj out [:nop op2]))
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
  [{edit1 :edit path1 :path} {edit2 :edit path2 :path}]
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
         [[:retain p1] [:delete p2]] (conj out [:nop op2])
         [[:delete p1] [:retain p2]] (conj out [op1 :nop])
         :else (conj out [:nop :nop])))]
    (->> (align-transform edit1 edit2)
         (reduce compare-ops [])
         pack-pairs)))

(defn ^:private align-compose
  [edit1 edit2]
  (loop [edit1 edit1 edit2 edit2 out []]
    (let [op1 (first edit1) op2 (first edit2)]
      (cond
        (nil? op1) (concat out (map #(vector :nop %) edit2))
        (nil? op2) (concat out (map #(vector % :nop) edit1))
        (= :delete (first op1)) (recur (rest edit1) edit2 (conj out [op1 :nop]))
        (= :insert (first op2)) (recur edit1 (rest edit2) (conj out [:nop op2]))

        :else
        (let [[o1 p1] op1
              p1' (if (seq? p1) (count p1) p1)
              [o2 p2] op2]
          (case (compare p1' p2)
            -1
            (recur (rest edit1) (cons [o2 (- p2 p1')] (rest edit2))
                   (conj out [op1 [o2 p1']]))
            0
            (recur (rest edit1) (rest edit2)
                   (conj out [op1 op2]))
            1
            (let [[left taken] (if (seq? p1)
                                 [(drop p2 p1) (take p2 p1)]
                                 [(- p1 p2) p2])]
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
           [:nop _   ] out
           [_    :nop] out))]
      (->> (align-compose edit1 edit2)
           (reduce compare-ops [])
           pack)))
  ([edit1 edit2 & more]
    (reduce compose (compose edit1 edit2) more)))

(defn transform-key
  [key edit]
  (loop [key key, index 0, edit edit]
    (if (empty? edit)
      key
      (let [[o p :as op] (first edit)]
        (case o
          :retain
          (recur key
                 (+ index p)
                 (rest edit))
          :delete
          (recur (if (> key index)
                   (max (- key p) 0)
                   key)
                 index
                 (rest edit))
          :insert
          (let [edit-count (count p)]
            (recur (if (>= key index)
                     (+ key edit-count)
                     key)
                   (+ index edit-count)
                   (rest edit))))))))

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
  (println (pr-str {:a a :b b :pre pre :suf suf}))
  (pack {:edit [[:retain pre]
                [:delete (- (count a) pre suf)]
                [:insert (subvec b pre (- (count b) suf))]
                [:retain suf]]}))

(defn diff
  [a b]
  (if (= a b)
    {:edit [[:retain (count a)]]}
    (let [a (vec a)
          b (vec b)
          pre (common-prefix a b)
          suf (common-suffix a b)]
      (if (< pre suf)
        (let [a' (subvec a 0 (- (count a) suf))
              b' (subvec b 0 (- (count b) suf))
              pre (common-prefix a' b')]
          (create-edit a b pre suf))
        (let [a' (subvec a pre)
              b' (subvec b pre)
              suf (common-suffix a' b')]
          (create-edit a b pre suf))))))
