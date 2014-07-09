(ns transom.core
  (:require [clojure.set :as set]
            [transom.protocols :as impl]
            [transom.string :as string]
            [transom.sequential :as vector])
  #+clj
  (:import (clojure.lang PersistentVector)))

(extend-protocol impl/Patchable
  #+clj java.lang.String
  #+cljs string
  (patch [this edit]
    (string/patch this edit))

  PersistentVector
  (patch [this edit]
    (vector/patch this edit)))

(extend-protocol impl/Diffable
  #+clj java.lang.String
  #+cljs string
  (diff [this that]
    (string/diff this that))

  PersistentVector
  (diff [this edit]
    (vector/diff this edit)))

(extend-protocol impl/WithRebasableKey
  #+clj java.lang.String
  #+cljs string
  (rebase-key [this key edit destructive?]
    (string/transform-caret key edit))

  PersistentVector
  (rebase-key [this key edit destructive?]
    (vector/transform-key key edit destructive?)))

(extend-protocol impl/WithComposableEdit
  #+clj java.lang.String
  #+cljs string
  (compose [this old-edit new-edit]
    (string/compose old-edit new-edit))

  PersistentVector
  (compose [this old-edit new-edit]
    (vector/compose old-edit new-edit)))

(extend-protocol impl/WithTransformableEdit
  #+clj java.lang.String
  #+cljs string
  (transform [this our-edit their-edit]
    (string/transform our-edit their-edit))
  
  PersistentVector
  (rebase [this our-edit their-edit]
    (vector/transform our-edit their-edit)))

(defn patch
  ([doc edit-map]
    (reduce
      (fn [state [path edit]] (update-in state path impl/patch edit))
      doc
      (sort-by (comp count key) edit-map)))
  ([doc edit-map & edit-maps]
    (reduce patch (patch doc edit-map) edit-maps)))

(def diff impl/diff)

(defn prefixes?
  [path-1 path-2]
  (and (< (count path-1) (count path-2))
       (= path-1 (subvec path-2 0 (count path-1)))))

(defn mappings-for
  [doc old new]
  (into {}
    (for [[new-path new-edit] new
          old-path (filter #(prefixes? new-path %) (keys old))]
      (let [state (get-in doc new-path)
            key-index (count new-path)
            key (nth old-path key-index)
            rebased-key (impl/rebase-key state key new-edit true)
            rebased-path (when rebased-key (assoc old-path key-index rebased-key))]
        [old-path rebased-path]))))

(defn compose
  ([doc old new]
    (let [mappings (mappings-for doc old new)
          rebased (set/rename-keys old mappings)
          rebased (dissoc rebased nil)]
      (reduce
        (fn [composed [new-path new-edit]]
          (if (contains? composed new-path)
            (assoc composed new-path
                   (impl/compose (get-in doc new-path)
                                 (get composed new-path)
                                 new-edit))
            (assoc composed new-path new-edit)))
        rebased
        new)))

  ([doc old new & more]
    (reduce (partial compose doc) (compose doc old new) more)))

#_(defn transform
  [doc ours theirs]
  ;; for each level until the max level, where level is length of each path
  ;;   find all paths in ours and theirs at that level
  ;;   divide these paths into exclusively ours, exclusively theirs, and shared
  ;;   rebase ours and theirs against each other,
  ;;   transform shared,
  ;; return modified ours and theirs
  (let [max-level (apply max (map count (concat (keys ours) (keys theirs))))]
    (loop [level 0
           ours' ours
           theirs' theirs]
      (if (> level max-level)
        [ours' theirs']
        (let [l (set (filter #(= (count %) level) (keys ours)))
              r (set (filter #(= (count %) level) (keys theirs)))
              shared (set/union l r)
              l' (set/difference l r)
              r' (set/difference r l)

              ;; something else should happen here
              [ours' theirs']
              (reduce
                (fn [[ours theirs] path]
                  (let [state (get-in doc path)
                        our-edit (get ours path)
                        their-edit (get theirs path)
                        [our-edit' their-edit'] (impl/transform state our-edit their-edit)]
                    [(assoc ours path our-edit') (assoc theirs path their-edit')]))
                [ours' theirs']
                shared)]
          (recur (inc level) ours' theirs'))))))
