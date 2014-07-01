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
        edit-3 {[:strings 1] [[:delete 1]]}
        edit-4 {[:strings] [[:delete 1] [:retain 2]]}
        result-1 {[:strings 0] [[:insert "yod"] [:retain 1] [:insert " is dead"]]}]
    (is (= (transom/compose doc edit-1 edit-2)
           result-1))
    (is (= (transom/compose doc edit-1 edit-3)
           (merge edit-1 edit-3)))
    (is (= (transom/compose doc edit-1 edit-2 edit-3)
           (merge result-1 edit-3)))
    (is (= (transom/compose doc edit-1 edit-4)
           edit-4))
    (is (= (transom/compose doc edit-3 edit-4)
           {[:strings] [[:delete 1] [:retain 2]]
            [:strings 0] [[:delete 1]]}))))
