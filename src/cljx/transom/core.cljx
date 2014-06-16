(ns transom.core
  (:require [clojure.set :as set]
            [transom.protocols :as impl :refer [Diffable]]
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

;; clojure.set/rename-keys is buggy in clojurescript :/
(defn rename-keys
  [map kmap]
  (reduce
    (fn [m [old new]]
      (if (contains? map old)
        (assoc m new (get map old))
        m))
    (apply dissoc map (keys kmap))
    kmap))

(defn rename-keys-with
  [f m]
  (let [kmap (reduce (fn [kmap k] (assoc kmap k (f k))) {} (keys m))]
    (rename-keys m kmap)))

(defn prefixes?
  [path-1 path-2]
  (and (< (count path-1) (count path-2))
       (= path-1 (subvec path-2 0 (count path-1)))))

(defn rebase-path-keys
  [doc old new]
  (letfn
    [(rebase-path
       [old-path new-path]
       (let [state (get-in doc new-path)
             new-edit (get new new-path)
             key-index (count new-path)
             key (nth old-path key-index)]
         (when-let [rebased-key (impl/rebase-key state key new-edit true)]
           (assoc old-path key-index rebased-key))))
     (path-fn
       [path]
       (reduce (fn [path new-path]
                 (when path
                   (when-let [path' (rebase-path path new-path)]
                     path')))
               path
               (filter #(prefixes? % path) (keys new))))]
    (-> (rename-keys-with path-fn old)
        (dissoc nil))))

(defn shared-keys
  [m1 m2]
  (set/intersection (set (keys m1)) (set (keys m2))))

(defn compose
  [doc old new]
  (let [rebased (rebase-path-keys doc old new)]
    (reduce
      (fn [rebased [new-path new-edit :as new-entry]]
        (if (contains? rebased new-path)
          (assoc rebased new-path (impl/compose (get-in doc new-path)
                                                (get rebased new-path)
                                                new-edit))
          
          (conj rebased new-entry)))
      rebased
      new)))

(defn transform
  [doc ours theirs]
  (let [ours' (rebase-path-keys doc ours theirs)
        theirs' (rebase-path-keys doc theirs ours)
        shared-paths (shared-keys ours' theirs')]
    (reduce
      (fn [[ours theirs] path]
        (let [state (get-in doc path)
              our-edit (get ours path)
              their-edit (get theirs path)
              [our-edit' their-edit'] (impl/transform state our-edit their-edit)]
          [(assoc ours path our-edit') (assoc theirs path their-edit')]))
      [ours' theirs']
      shared-paths)))
