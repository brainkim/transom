(ns transom.document
  (:require [transom.core :as transom]
            [transom.utils :refer [dissocv]])
  #+clj
  (:import (clojure.lang IPersistentVector)))

(defprotocol IDocument
  (patch [this edit version])
  (update [this path new-value]))

(defprotocol IHistory
  (version [this])
  (collect [this version])
  (add [this edit]))

(defrecord Document [state history]
  IHistory
  (version [_] (version history))
  (collect [_ version] (collect history version))
  (add [_ edit] (add history edit))
  IDocument
  (patch [this edit version]
    (let [edits (collect history version)
          edit (reduce
                 (fn [yours mine] (second (transom/transform state mine yours)))
                 edit
                 edits)]
      (Document. (transom/patch state edit) (add history edit))))
  (update [this path new-value]
    (let [old-value (if (empty? path) state (get-in state path))
          new-state (if (empty? path) new-value (assoc-in state path new-value))
          edit (transom/diff old-value new-value)]
      (Document. new-state (add history {path edit})))))

(extend-type #+clj IPersistentVector #+cljs PersistentVector
  IHistory
  (version [this] (count this))
  (collect [this version] (subvec this version))
  (add [this edit] (conj this edit)))

(defn document
  []
  (atom (Document. {} [])))

(defn update!
  [doc path new-value]
  (swap! doc update path new-value))

(defn transact!
  [doc path f & args]
  (swap! doc (fn [doc] (update doc path (apply f (get-in (:state doc) path) args)))))

;; TODO(brian): Do I need to sort paths before deleting? Ugh my brain.
(defn delete!
  ([doc path]
   (assert (vector? path) "Paths are always vectors!")
   (swap! doc
     (fn [doc]
       (let [state (:state doc)
             target-ref (peek path)
             target-path (pop path)
             target (get-in state target-path)
             target' (cond (vector? target) (dissocv target target-ref)
                           (map? target) (dissoc target target-ref))]
         (update doc target-path target')))))
  ([doc path & paths]
    ;; TODO(brian): batch edits together so they don't muck up history?
    (reduce delete! doc (delete! doc path) paths)))

(defn patch!
  [doc edit version]
  (swap! doc patch edit version))

(comment
  (def pizza (document))
  (get @pizza :state)
  (get @pizza :history)
  (deref pizza)
  (update! pizza [] {:foo "bar" :diff "please" :bar "baz"})
  (update! pizza [:foo] "barbies")
  (transact! pizza [:foo] (fn [foo] (str "what is the deal " foo " are the best")))
  (transact! pizza [] (fn [old] (assoc old :vec ["a"])))
  (transact! pizza [:vec] (fn [v] (vec (remove #(= % "a") v))))
  (transact! pizza [:vec] (fn [v] (into v ["foo" "bar" ",,"])))
  (apply transom/compose nil (:history @pizza))
  )
