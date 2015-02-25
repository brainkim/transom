(ns transom.transom
  (:require [clojure.set :as set]
            [transom.protocols :as impl]
            [transom.string :as ts]
            [transom.vector :as tv]
            [transom.map :as tm]
      #+clj [transom.macros :refer [extend-protocols]])
  #+cljs
  (:require-macros [transom.macros :refer [extend-protocols]])
  #+clj
  (:import (java.lang String)
           (clojure.lang IPersistentVector)
           (clojure.lang IPersistentMap)))

(extend-protocols impl/Diffable
  [#+clj String #+cljs string]
  (diff [this that] (ts/diff this that))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (diff [this that] (tv/diff this that))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (diff [this that] (tm/diff this that)))

(extend-protocols impl/Patchable
  [#+clj String #+cljs string]
  (patch [this edit] (ts/patch this edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (patch [this edit] (tv/patch this edit))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (patch [this edit] (tm/patch this edit)))

(extend-protocols impl/WithInvertibleEdit
  [#+clj String #+cljs string]
  (invert [this edit] (ts/invert edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (invert [this edit] (tv/invert edit))

  [#+clj IPersistentMap]
  (invert [this edit] (tm/invert edit)))

(extend-protocols impl/WithRebasableRef
  [#+clj String #+cljs string]
  (rebase-ref [this key edit destructive?] (ts/transform-caret key edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (rebase-ref [this key edit destructive?] (tv/transform-key key edit destructive?))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (rebase-ref [this key edit destructive?] (tm/transform-key key edit)))

(extend-protocols impl/WithComposableEdit
  [#+clj String #+cljs string]
  (compose [this old-edit new-edit] (ts/compose old-edit new-edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (compose [this old-edit new-edit] (tv/compose old-edit new-edit))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (compose [this old-edit new-edit] (tm/compose old-edit new-edit)))

(extend-protocols impl/WithTransformableEdit
  [#+clj String #+cljs string]
  (transform [this my-edit your-edit] (ts/transform my-edit your-edit))

  [#+clj IPersistentVector #+cljs PersistentVector]
  (transform [this my-edit your-edit] (tv/transform my-edit your-edit))

  [#+clj IPersistentMap #+cljs PersistentHashMap #+cljs PersistentArrayMap]
  (transform [this my-edit your-edit] (tm/transform my-edit your-edit)))

(defn diff
  [old new]
  (assert (= (type old) (type new)))
  (impl/diff old new))

(defn patch
  ([doc edit-map]
    (reduce
      (fn [doc [path edit]]
        (if (empty? path)
          (impl/patch doc edit)
          (update-in doc path impl/patch edit)))
      doc
      (sort-by (comp count key) edit-map)))
  ([doc edit-map & edit-maps]
    (reduce patch (patch doc edit-map) edit-maps)))

(defn unpatch
  ([doc edit-map]
    (reduce
      (fn [doc [path edit]]
        (if (empty? path)
          (impl/patch doc (impl/invert doc edit))
          (let [inverted (impl/invert (get-in doc path) edit)]
            (update-in doc path impl/patch inverted))))
      doc
      (sort-by (comp - count key) edit-map))))

(defn ^:private prefixes?
  [path-1 path-2]
  (and (< (count path-1) (count path-2))
       (= path-1 (subvec path-2 0 (count path-1)))))

(defn ^:private group-fn
  [[old new]]
  (cond
    (nil? new) :cleared
    (= old new) :unchanged
    :else :changed))

(defn ^:private rename-key
  [m a b]
  (-> m
      (assoc b (get m a))
      (dissoc a)))

;; TODO(brian): I smell a beautiful tail-recursive function here
(defn rebase-compose
  [doc old new]
  (let [!old (atom old), !new (atom new)]
    (doseq [[new-path new-edit] new
            [old-path old-edit] old
            :when (prefixes? new-path old-path)]
      (let [doc' (get-in doc new-path)
            k-index (count new-path)
            k (nth old-path k-index)
            reb-k (impl/rebase-ref doc' k new-edit true)]
        (if (nil? reb-k)
          (let [old-path' (subvec old-path (count new-path))]
            (swap! !old dissoc old-path)
            (swap! !new assoc new-path
              (impl/diff
                (unpatch doc' {old-path' old-edit})
                (impl/patch doc' new-edit))))
          (let [reb-path (assoc old-path k-index reb-k)]
            (swap! !old rename-key old-path reb-path)))))
    [@!old @!new]))

(defn compose
  ([doc old new]
    (let [doc (patch doc old)
          [old new] (rebase-compose doc old new)]
      (reduce
        (fn [old [new-path new-edit]]
          (let [old-edit (get old new-path)]
            (if (not (nil? old-edit))
              (let [doc' (get-in doc new-path)
                    cmp-edit (impl/compose doc' old-edit new-edit)]
                (assoc old new-path cmp-edit))
              (assoc old new-path new-edit))))
        old
        new)))
  ([doc old new & more]
    (reduce (partial compose doc) (compose doc old new) more)))

;; TODO(brian): Reduce code dup with rebase-compose
(defn ^:private rebase-transform
  [doc in ex]
  (let [!in (atom in), !ex (atom ex)]
    (doseq [[ex-path ex-edit] ex
            [in-path in-edit] in
            :when (prefixes? ex-path in-path)]
      (let [doc' (get-in doc ex-path)
            k-index (count ex-path)
            k (nth in-path k-index)
            reb-k (impl/rebase-ref doc' k ex-edit true)]
        (if (nil? reb-k)
          (let [in-path' (subvec in-path (count ex-path))
                doc'' (get-in doc' in-path')]
            (swap! !in dissoc in-path)
            (swap! !ex assoc ex-path
              (impl/diff
                (impl/patch doc'' in-edit)
                (impl/patch doc' ex-edit))))
          (let [reb-path (assoc in-path k-index reb-k)]
            (swap! !in rename-key in-path reb-path)))))
    [@!in @!ex]))

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

              [mine' yours']
              (transform-shared doc mine yours shared-paths)

              [mine'' yours''']
              (rebase-transform doc mine' (select-keys yours' your-paths))

              [yours'' mine''']
              (rebase-transform doc yours' (select-keys mine' my-paths))]
          [(merge mine'' mine''') (merge yours'' yours''')]))
      [mine yours]
      (range (inc max-level)))))
