(ns transom.hash-map-test
  (:require [transom.hash-map :as thm]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest diff-test
  (is (= {:inserts {:baz 4}
          :updates {:foo [1 3]}
          :deletes {:bar 2}}
         (thm/diff {:foo 1 :bar 2} {:foo 3 :baz 4}))))

(deftest patch-test
  (is (= {:foo 3 :baz 4}
         (thm/patch {:foo 1 :bar 2}
                    {:inserts {:baz 4}
                     :updates {:foo [1 3]}
                     :deletes {:bar 2}}))))
