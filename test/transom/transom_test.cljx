(ns transom.transom-test
  (:require [transom.transom :as transom]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(comment
  ;; WIP(brian): generative tests for path logic
(defn aseq
  [a]
  (cond
    (sequential? a) (map-indexed vector a)
    (associative? a) (seq a)))

(defn paths-in
  [a]
  (if (associative? a)
    (concat
      [[]]
      (mapcat
        (fn [[k v]]
          (concat
            [[k]]
            (for [path (paths-in v) :when (seq path)]
              (into [k] path))))
        (aseq a)))
    []))

(defn contains-in?
  [coll [k & ks]]
  (if ks
    (and (contains? coll k) (contains-in? (get coll k) ks))
    (or (nil? k) (contains? coll k))))

;; Generators
(def associative
  (gen/recursive-gen
    (fn [s] (gen/one-of [(gen/vector s) (gen/map s s)]))
    gen/string-alphanumeric))

;; Only takes generators of strings, maps, and vectors
(defn same-type
  [v]
  (cond
    (string? v) gen/string-alphanumeric
    (map? v) (gen/map gen/string-alphanumeric gen/string-alphanumeric)
    (vector? v) (gen/vector gen/string-alphanumeric)))

(defn diff-gen
  [gen]
  (gen/bind gen
    (fn [g]
      (gen/bind (same-type g)
        (fn [g']
          (transom/diff g g'))))))

(defn with-paths
  ([a-gen] (with-paths a-gen 1))
  ([a-gen num-paths]
    (gen/fmap
      (fn [a]
        (cons
          a
          (gen/sample (gen/elements (paths-in a)) num-paths)))
      a-gen)))

(defspec a-contains-all-paths
  1000
  (prop/for-all [a associative]
    (every? (partial contains-in? a)
      (paths-in a))))

(def composed-edits
  (gen/bind (with-paths associative)
    (fn [a path]
      (let [a' (get-in a path)
            diff-gen (gen/bind (same-type a')
                       (fn [a''] (transom/diff a' a'')))]
        (gen/bind diff-gen
          (fn [diff]
            (gen/return 1)))))))
  )

(deftest unpatch-test
  (let [doc {1 ["abc"]}
        edit {[] {2 [:insert 0]}
              [1] [[:retain 1] [:insert ["xyz"]]]
              [1 1] [[:delete "xyz"]]}]
    (is (= doc (transom/unpatch (transom/patch doc edit) edit)))))

(deftest compose-test
  (let [doc {:a ["a" "b" "c"]}
        edit-1 {[:a 0] [[:insert "yod"] [:retain 1]]}
        edit-2 {[:a 0] [[:retain 4] [:insert " is dead"]]}
        edit-1-and-2 {[:a 0] [[:insert "yod"] [:retain 1] [:insert " is dead"]]} 
        edit-3 {[:a 1] [[:delete "b"]]}
        edit-3' {[:a 0] [[:delete "b"]]}
        edit-4 {[:a] [[:delete ["a"]] [:retain 2]]}
        edit-4' {[:a] [[:delete ["yoda"]] [:retain 2]]}
        edit-4'' {[:a] [[:delete ["yoda is dead"]] [:retain 2]]}]
    (is (= edit-1
           (transom/compose doc edit-1 {})))
    (is (= edit-1
           (transom/compose doc {} edit-1)))
    (is (= edit-1-and-2
           (transom/compose doc edit-1 edit-2)))
    (is (= (merge edit-1 edit-3)
           (transom/compose doc edit-1 edit-3)))
    (is (= (merge edit-1-and-2 edit-3)
           (transom/compose doc edit-1 edit-2 edit-3)))
    (is (= edit-4
           (transom/compose doc edit-1 edit-4')))
    (is (= (merge edit-1 edit-4)
           (transom/compose doc edit-4 edit-1)))
    (is (= (merge edit-3' edit-4)
           (transom/compose doc edit-3 edit-4)))
    (is (= edit-4
           (transom/compose doc edit-1 edit-2 edit-4'')))
    (is (= (merge edit-4 edit-1-and-2)
           (transom/compose doc edit-4 edit-1 edit-2)))
    (is (= {[:a] [[:retain 3] [:insert [""]]]
            [:a 3] [[:insert "orange is the new black"]]}
           (transom/compose doc
                            {[:a] [[:retain 3] [:insert [""]]]
                             [:a 3] [[:insert "orange"]]}
                            {[:a 3] [[:retain 6] [:insert " is the new black"]]})))
    (is (= {[:a] [[:retain 3] [:insert [""]]]
            [:a 3] [[:insert "orange is the new black"]]}
           (transom/compose doc
                            {[:a] [[:retain 3] [:insert [""]]]}
                            {[:a 3] [[:insert "orange"]]}
                            {[:a 3] [[:retain 6] [:insert " is the new black"]]})))))

(deftest deep-compose-test
  (is (= {[] {:a [:insert [""]]}
          [:a] [[:retain 1] [:insert ["a" "b" "c"]]]
          [:a 0] [[:insert "bba"]]}
          (transom/compose {}
                           {[] {:a [:insert [""]]}}
                           {[:a] [[:retain 1] [:insert ["a" "b" "c"]]]}
                           {[:a 0] [[:insert "bba"]]})))
  #_(is (= {}
         (transom/compose {:a "poop"}
                          {[:a] [[:retain 4] [:insert " in my pants"]]}
                          {[] {:a [:update "poop in my pants" "poop"]}}))))

(deftest transform-test
  (let [doc {:a ["s" "sa" "sad"]}
        edit-1 {[:a 0] [[:insert "a"] [:retain 1]]}
        edit-2 {[:a 0] [[:delete "s"]]}
        edit-1' {[:a 0] [[:insert "a"]]}
        edit-2' {[:a 0] [[:retain 1] [:delete "s"]]}]
    (is (= [edit-1 {}]
           (transom/transform doc edit-1 {})))
    (is (= [{} edit-1]
           (transom/transform doc {} edit-1)))
    (is (= [edit-1' edit-2']
           (transom/transform doc edit-1 edit-2)))))
