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
        edit-3 {[:strings 1] [[:delete "b"]]}
        edit-3' {[:strings 0] [[:delete "b"]]}
        edit-4 {[:strings] [[:delete ["a"]] [:retain 2]]}]
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
           (transom/compose doc edit-1 edit-4)))
    (is (= (merge edit-1 edit-4)
           (transom/compose doc edit-4 edit-1)))
    (is (= (merge edit-3' edit-4)
           (transom/compose doc edit-3 edit-4)))
    (is (= edit-4
           (transom/compose doc edit-1 edit-2 edit-4)))
    (is (= (merge edit-4 edit-1-and-2)
           (transom/compose doc edit-4 edit-1 edit-2)))
    (is (= {[:strings] [[:retain 3] [:insert [""]]]
            [:strings 3] [[:insert "orange is the new black"]]}
           (transom/compose doc
                            {[:strings] [[:retain 3] [:insert [""]]]
                             [:strings 3] [[:insert "orange"]]}
                            {[:strings 3] [[:retain 6] [:insert " is the new black"]]})))
    (is (= {[:strings] [[:retain 3] [:insert [""]]]
            [:strings 3] [[:insert "orange is the new black"]]}
           (transom/compose doc
                            {[:strings] [[:retain 3] [:insert [""]]]}
                            {[:strings 3] [[:insert "orange"]]}
                            {[:strings 3] [[:retain 6] [:insert " is the new black"]]})))))

(deftest deep-compose-test
  (let [doc {}]
    (is (= {[] {:strings [:insert []]}
            [:strings] [[:insert ["a" "b" "c"]]]
            [:strings 0] [[:retain 1] [:insert "bba"]]}
           (transom/compose doc
                            {[] {:strings [:insert []]}}
                            {[:strings] [[:insert ["a" "b" "c"]]]}
                            {[:strings 0] [[:retain 1] [:insert "bba"]]})))))

(deftest transform-test
  (let [doc {:strings ["s" "sa" "sad"]}
        edit-1 {[:strings 0] [[:insert "a"] [:retain 1]]}
        edit-2 {[:strings 0] [[:delete "s"]]}
        edit-1' {[:strings 0] [[:insert "a"]]}
        edit-2' {[:strings 0] [[:retain 1] [:delete "s"]]}]
    (is (= [edit-1 {}]
           (transom/transform doc edit-1 {})))
    (is (= [{} edit-1]
           (transom/transform doc {} edit-1)))
    (is (= [edit-1' edit-2']
           (transom/transform doc edit-1 edit-2)))))
