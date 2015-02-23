(ns transom.transom-test
  (:require [transom.transom :as transom]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; TODO(brian): replace these tests with generative tests!
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
  (is (= {[] {:strings [:insert [""]]}
          [:strings] [[:retain 1] [:insert ["a" "b" "c"]]]
          [:strings 0] [[:insert "bba"]]}
          (transom/compose {}
                           {[] {:strings [:insert [""]]}}
                           {[:strings] [[:retain 1] [:insert ["a" "b" "c"]]]}
                           {[:strings 0] [[:insert "bba"]]})))
  #_(is (= {}
         (transom/compose {:strings "poop"}
                          {[:strings] [[:retain 4] [:insert " in my pants"]]}
                          {[] {:strings [:update "poop in my pants" "poop"]}}))))

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
