(ns transom.string
  (:require [transom.sequential :refer [count-before
                                        count-after
                                        compose-abstract
                                        transform-abstract]]))

(def ^:private nop? (some-fn nil? #(= (second %) 0) #(= (second %) "")))

(defn pack
  [edit]
  (let [edit (remove nop? edit)
        edit (map (fn [[o p :as op]] (if (= o :insert) [o (apply str p)] op)) edit)]
    (if (empty? edit)
      []
      (reduce
        (fn [edit op]
          (let [[o1 p1] (peek edit), [o2 p2] op]
            (if (= o1 o2)
              (conj (pop edit)
                    (case o1 :retain [:retain (+ p1 p2)]
                             :delete [:delete (+ p1 p2)]
                             :insert [:insert (str p1 p2)]))
              (conj edit op))))
        (vector (first edit))
        (rest edit)))))

(defn ^:private pack-pairs
  [pairs]
  (if (empty? pairs)
    pairs
    (map pack (apply map vector pairs))))

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

;; NOTE(brian): one thing I don't like about this edit representation is that a
;;   no-op is represented as an edit with a single :retain op. Having to keep
;;   track of :retains makes edits poorly formed.
;;   e.g. the empty edit could just as well be represented as [] or [[:retain 0]]
(defn diff
  [a b]
  (if (= a b)
    (pack [[:retain (count a)]]) ; don't forget to wrap it dummy
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

(defn compose
  ([edit1 edit2]
    (pack (compose-abstract edit1 edit2)))
  ([edit1 edit2 & more]
    (reduce compose (compose edit1 edit2) more)))

(defn transform [edit1 edit2] (pack-pairs (transform-abstract edit1 edit2)))

(defn transform-caret
  [caret edit]
  ;; caret is the little blinky line thing
  ;; index is where we are in the (new?) document
  (first
    (reduce
      (fn [[caret index] [o p]]
        (case o
          :retain [caret, (+ index p)]
          :delete [(max (- caret p) 0), index]
          :insert [(if (>= caret index) (+ caret (count p)) caret), (+ index (count p))]))
      [caret, 0]
      edit)))
