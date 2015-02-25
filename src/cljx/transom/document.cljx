(ns transom.document
  (:require [transom.transom :as transom]
            [transom.utils :refer [dissocv]])
  #+clj
  (:import (clojure.lang IPersistentVector)))

(defprotocol IDocument
  (latest [this])
  (patch [this edit version])
  (update [this path value version]))

(defprotocol IHistory
  (version [this])
  (collect [this version])
  (add [this edit]))

(defrecord Document [current history]
  IDocument
  (latest [this] [current (version history)])
  (patch [this edit version]
    (let [edits (collect history version)
          state (reduce transom/unpatch current edits)

          [state edit]
          (reduce
            (fn [[state edit] edit']
              (let [edit (second (transom/transform state edit edit'))]
                [(transom/patch state edit) edit]))
            [state edit]
            (reverse edits))]
      (Document. (transom/patch state edit) (add history edit))))
  (update [this path value version]
    (let [edits (collect history version)
          state (reduce transom/unpatch current edits)
          old-value (get-in state path)
          edit (transom/diff old-value value)]
      (prn edit)
      (patch this {path edit} version))))

(extend-type #+clj IPersistentVector #+cljs PersistentVector
  IHistory
  (version [this] (count this))
  (collect [this version] (rseq (subvec this version)))
  (add [this edit] (conj this edit)))

(defn document
  ([] (document {} []))
  ([initial] (document initial []))
  ([initial history]
    (Document. {} history)))

(comment
  (def pizza (atom (document {} [])))
  (latest @pizza)
  (swap! pizza update [] {:foo "bar"} 0)
  (swap! pizza update [] {:foo "bar"} 0)
  (update! pizza [] {:foo "bar" :diff "please" :bar "baz"})
  (update! pizza [:foo] "barbies")
  (transact! pizza [:foo] (fn [foo] (str "what is the deal " foo " are the best")))
  (transact! pizza [] (fn [old] (assoc old :vec ["a"])))
  (transact! pizza [:vec] (fn [v] (vec (remove #(= % "a") v))))
  (transact! pizza [:vec] (fn [v] (into v ["foo" "bar" ",,"])))
  (apply transom/compose nil (:history @pizza))
  )
