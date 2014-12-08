(ns transom.indexed-test
  (:require [transom.core :as transom]
            [transom.sequential :as ts]
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
    (and (= m2 (ts/patch m1 (ts/diff m1 m2)))
         (= m1 (ts/patch m2 (ts/diff m2 m1))))))

(defspec composing-edits
  100
  (prop/for-all [m1 simple-vector-gen
                 m2 simple-vector-gen
                 m3 simple-vector-gen]
    (let [edit1 (ts/diff m1 m2)
          edit2 (ts/diff m2 m3)]
      (= m3
         (ts/patch m1 edit1 edit2)
         (ts/patch m1 (ts/compose edit1 edit2))))))

(defspec transforming-edits
  100
  (prop/for-all [m1 simple-vector-gen
                 m2 simple-vector-gen
                 m3 simple-vector-gen]
    (let [edit1 (ts/diff m1 m2)
          edit2 (ts/diff m1 m3)
          [edit1' edit2'] (ts/transform edit1 edit2)]
      (= (ts/patch m1 edit1 edit2')
         (ts/patch m1 edit2 edit1')))))

#_(defspec composing-transforms
  100
  (prop/for-all [m1 simple-vector-gen
                 m2 simple-vector-gen
                 m3 simple-vector-gen]
    (let [old (ts/diff m1 m2)
          new (ts/diff m1 m3)
          [old' new'] (ts/transform old new)]
      (= (ts/compose old new') (ts/compose new old')))))

;; TODO(brian): invert not implemented
#_(defspec inverting-edits
  100
  (prop/for-all [m1 simple-vector-gen
                 m2 simple-vector-gen]
    (let [edit (ts/diff m1 m2)
          inverted (ts/invert edit)]
      (= m1 (ts/patch m1 edit inverted)))))
