(ns transom.core
  (:require [clojure.set :as set]
            [transom.protocols :as impl]
            [transom.string :as string]
            [transom.sequential :as vector]
            [transom.map :as map]
      #+clj [transom.macros :refer [extend-protocols]])
  #+cljs
  (:require-macros [transom.macros :refer [extend-protocols]])
  #+clj
  (:import (java.lang String)
           (clojure.lang IPersistentVector)
           (clojure.lang IPersistentMap)))

(extend-protocols impl/Diffable
  [#+clj String #+cljs string]
  (diff [this that]
    (string/diff this that))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (diff [this that]
    (vector/diff this that))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (diff [this that]
    (map/diff this that)))

(extend-protocols impl/Patchable
  [#+clj String #+cljs string]
  (patch [this edit]
    (string/patch this edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (patch [this edit]
    (vector/patch this edit))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (patch [this edit]
    (map/patch this edit)))

(extend-protocols impl/WithInvertibleEdit
  [#+clj IPersistentMap]
  (invert [this edit]
    (map/invert edit)))

(extend-protocols impl/WithRebasableRef
  [#+clj String #+cljs string]
  (rebase-ref [this key edit destructive?]
    (string/transform-caret key edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (rebase-ref [this key edit destructive?]
    (vector/transform-key key edit destructive?))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (rebase-ref [this key edit destructive?]
    (map/transform-key key edit)))

(extend-protocols impl/WithComposableEdit
  [#+clj String #+cljs string]
  (compose [this old-edit new-edit]
    (string/compose old-edit new-edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (compose [this old-edit new-edit]
    (vector/compose old-edit new-edit))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (compose [this old-edit new-edit]
    (map/compose old-edit new-edit)))

(extend-protocols impl/WithTransformableEdit
  [#+clj String #+cljs string]
  (transform [this my-edit your-edit]
    (string/transform my-edit your-edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (transform [this my-edit your-edit]
    (vector/transform my-edit your-edit))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (transform [this my-edit your-edit]
    (map/transform my-edit your-edit)))

(defn diff
  [old new]
  (assert (= (type old) (type new)))
  (impl/diff old new))

(defn patch
  ([doc edit-map]
    (reduce
      (fn [state [path edit]]
        (if (empty? path)
          (impl/patch doc edit)
          (update-in state path impl/patch edit)))
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
          old-path (keys old) :when (prefixes? new-path old-path)]
      (let [state (get-in doc new-path)
            ki (count new-path)
            k (nth old-path ki)
            reb-k (impl/rebase-ref state k new-edit true)
            reb-path (when reb-k (assoc old-path ki reb-k))]
        [old-path reb-path]))))

(defn rebase-paths
  [doc old new]
  (let [mappings (rebase-mappings doc old new)
        reb (set/rename-keys old mappings)]
    (dissoc reb nil)))

(defn compose
  ([doc old new]
    (let [doc (patch doc old)
          reb (rebase-paths doc old new)]
      (reduce
        (fn [reb [new-path new-edit]]
          (if-some [reb-edit (get reb new-path)]
            (let [state (get-in doc new-path)
                  cmp-edit (impl/compose state reb-edit new-edit)]
              (assoc reb new-path cmp-edit))
            (assoc reb new-path new-edit)))
        reb
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
    (reduce
      (fn [[mine yours] level]
        (let [ffn #(= (count %) level)
              my-paths (filter ffn (keys mine))
              your-paths (filter ffn (keys yours))
              shared-paths (set/intersection (set my-paths) (set your-paths))
              [mine yours] (transform-shared doc mine yours shared-paths)
              mine' (rebase-paths doc mine (select-keys yours your-paths))
              yours' (rebase-paths doc yours (select-keys mine my-paths))]
          [mine' yours']))
      [mine yours]
      (range (inc max-level)))))
