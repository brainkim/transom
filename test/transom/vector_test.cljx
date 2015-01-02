(ns transom.vector-test
  (:require [transom.core :as transom]
            [transom.vector :as tv]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def simple-vector-gen (gen/vector gen/int))

(defspec patching-diffs
  100
  (prop/for-all [m1 simple-vector-gen
                 m2 simple-vector-gen]
    (and (= m2 (tv/patch m1 (tv/diff m1 m2)))
         (= m1 (tv/patch m2 (tv/diff m2 m1))))))

(defspec composing-edits
  100
  (prop/for-all [m1 simple-vector-gen
                 m2 simple-vector-gen
                 m3 simple-vector-gen]
    (let [edit1 (tv/diff m1 m2)
          edit2 (tv/diff m2 m3)]
      (= m3
         (tv/patch m1 edit1 edit2)
         (tv/patch m1 (tv/compose edit1 edit2))))))

(defspec transforming-edits
  100
  (prop/for-all [m1 simple-vector-gen
                 m2 simple-vector-gen
                 m3 simple-vector-gen]
    (let [edit1 (tv/diff m1 m2)
          edit2 (tv/diff m1 m3)
          [edit1' edit2'] (tv/transform edit1 edit2)]
      (= (tv/patch m1 edit1 edit2') (tv/patch m1 edit2 edit1')))))
