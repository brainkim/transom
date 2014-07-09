(ns transom.core-test
  (:require [transom.core :as transom]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest compose-test
  (let [doc {:strings ["a" "b" "c"]}
        edit-1 {[:strings 0] [[:insert "yod"] [:retain 1]]}
        edit-2 {[:strings 0] [[:retain 4] [:insert " is dead"]]}
        edit-1-and-2 {[:strings 0] [[:insert "yod"] [:retain 1] [:insert " is dead"]]} 
        edit-3 {[:strings 1] [[:delete 1]]}
        edit-3' {[:strings 0] [[:delete 1]]}
        edit-4 {[:strings] [[:delete 1] [:retain 2]]}]
    (are [x y] (= x y)
      (transom/compose doc edit-1 {})
      edit-1

      (transom/compose doc {} edit-1)
      edit-1

      (transom/compose doc edit-1 edit-2)
      edit-1-and-2

      (transom/compose doc edit-1 edit-3)
      (merge edit-1 edit-3)

      (transom/compose doc edit-1 edit-2 edit-3)
      (merge edit-1-and-2 edit-3)

      (transom/compose doc edit-1 edit-4)
      edit-4

      (transom/compose doc edit-4 edit-1)
      (merge edit-1 edit-4)

      (transom/compose doc edit-3 edit-4)
      (merge edit-3' edit-4)

      (transom/compose doc edit-1 edit-2 edit-4)
      edit-4

      (transom/compose doc edit-4 edit-1 edit-2)
      (merge edit-4 edit-1-and-2))))

#_(deftest transform-test
  (let [doc {:strings ["a" "b" "c"]}
        edit-1 {[:strings 0] [[:insert "yod"] [:retain 1]]}
        edit-2 {[:strings 0] [[:delete 1]]}
        edit-1' {[:strings 0] [[:insert "yod"]]}
        edit-2' {[:strings 0] [[:retain 3] [:delete 1]]}]
    (are [x y] (= x y)
      (transom/transform doc edit-1 {})
      [edit-1 {}]

      (transom/transform doc edit-1 edit-2)
      [edit-1' edit-2']

      (transom/transform )
    )))
