(ns transom.core
  (:require [transom.protocols :as impl :refer [Diffable]]
            [transom.path :as path] 
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

(extend-protocol impl/WithRebaseableEdit
  #+clj java.lang.String
  #+cljs string
  (rebase [this our-edit their-edit]
    nil)
  
  PersistentVector
  (rebase [this our-edit their-edit]
    nil))

(extend-protocol impl/WithRebaseableKey
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

(defn patch
  [doc edit-map]
  (reduce
    (fn [state [path edit]] (update-in state path impl/patch edit))
    doc
    (sort-by path/generality edit-map)))

(def diff impl/diff)

(defn rebase-pair
  [doc our-edit-map [their-path their-edit]])

(defn rebase
  [doc our-edit-map their-edit-map]) 

(defn compose-pair
  [doc old-edit-map [new-path new-edit]]
  (let
    [edit-map
     (into {}
       (for [[old-path old-edit :as old-pair] old-edit-map]
         (cond
           (= old-path new-path)
           [old-path (impl/compose (get-in doc new-path) old-edit new-edit)]

           (path/prefix? new-path old-path)
           (let [suffix (path/suffix new-path old-path)]
             (when-let [rebased-key (impl/rebase-key (get-in doc new-path) (first suffix) new-edit true)]
               [(concat new-path (cons rebased-key (rest suffix))) old-edit]))

           :else old-pair)))]
    (if (contains? edit-map new-path)
      edit-map
      (assoc edit-map new-path new-edit))))

(defn compose
  [doc old-edit-map new-edit-map]
  (reduce (partial compose-pair doc) old-edit-map new-edit-map))
