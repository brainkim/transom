(ns transom.core
  (:require [clojure.set :as set]
            [transom.protocols :as impl]
            [transom.string :as string]
            [transom.sequential :as vector])
  #+clj
  (:import (clojure.lang PersistentVector)))

(extend-protocol impl/Diffable
  #+clj java.lang.String
  #+cljs string
  (diff [this that]
    (string/diff this that))

  PersistentVector
  (diff [this edit]
    (vector/diff this edit)))

(extend-protocol impl/Patchable
  #+clj java.lang.String
  #+cljs string
  (patch [this edit]
    (string/patch this edit))

  PersistentVector
  (patch [this edit]
    (vector/patch this edit)))

(extend-protocol impl/WithRebasableRef
  #+clj java.lang.String
  #+cljs string
  (rebase-ref [this key edit destructive?]
    (string/transform-caret key edit))

  PersistentVector
  (rebase-ref [this key edit destructive?]
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
  (transform [this my-edit your-edit]
    (string/transform my-edit your-edit))
  
  PersistentVector
  (transform [this my-edit your-edit]
    (vector/transform my-edit your-edit)))

(def diff impl/diff)

(defn patch
  ([doc edit-map]
    (reduce
      (fn [state [path edit]] (update-in state path impl/patch edit))
      doc
      (sort-by (comp count key) edit-map)))
  ([doc edit-map & edit-maps]
    (reduce patch (patch doc edit-map) edit-maps)))

(defn ^:private prefixes?
  [path-1 path-2]
  (and (< (count path-1) (count path-2))
       (= path-1 (subvec path-2 0 (count path-1)))))

(defn ^:private rebase-mappings
  [doc old new]
  (into {}
    (for [[new-path new-edit] new
          old-path (keys old)
          :when (prefixes? new-path old-path)]
      (let [state (get-in doc new-path)
            key-index (count new-path)
            key (nth old-path key-index)
            reb-key (impl/rebase-ref state key new-edit true)
            reb-path (when reb-key (assoc old-path key-index reb-key))]
        [old-path reb-path]))))

(defn rebase-paths
  [doc old new]
  (let [mappings (rebase-mappings doc old new)
        rebased (set/rename-keys old mappings)]
    (dissoc rebased nil)))

(defn compose
  ([doc old new]
    (let [doc (patch doc old)
          old (rebase-paths doc old new)]
      (reduce
        (fn [old [new-path new-edit]]
          (if-some [old-edit (get old new-path)]
            (let [state (get-in doc new-path)
                  composed-edit (impl/compose state old-edit new-edit)]
              (assoc old new-path composed-edit))
            (assoc old new-path new-edit)))
        old
        new)))
  ([doc old new & more]
    (reduce (partial compose doc) (compose doc old new) more)))

(defn ^:private transform-shared
  [doc mine yours shared-paths]
  (reduce
    (fn [[mine yours] path]
      (let [state (get-in doc path)
            my-edit (get mine path)
            your-edit (get yours path)
            [my-edit your-edit] (impl/transform state my-edit your-edit)]
        [(assoc mine path my-edit) (assoc yours path your-edit)]))
    [mine yours]
    shared-paths))

(defn transform
  [doc mine yours]
  (let [max-level (apply max (map count (concat (keys mine) (keys yours))))]
    (loop [level 0, mine mine, yours yours]
      (if (> level max-level)
        [mine yours]
        (let [ffn #(= (count %) level)
              my-paths (filter ffn (keys mine))
              your-paths (filter ffn (keys yours))
              shared-paths (set/intersection (set my-paths) (set your-paths))
              [mine yours] (transform-shared doc mine yours shared-paths)
              mine' (rebase-paths doc mine (select-keys yours your-paths))
              yours' (rebase-paths doc yours (select-keys mine my-paths))]
          (recur (inc level) mine' yours'))))))
