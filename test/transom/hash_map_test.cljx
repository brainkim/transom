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
                     :deletes {:bar 2}})))
  (is (thrown? AssertionError (thm/patch {:a 1} {:inserts {:a 1}})))
  (is (thrown? AssertionError (thm/patch {:a 1} {:updates {:a [2 3]}})))
  (is (thrown? AssertionError (thm/patch {:a 1} {:deletes {:a 2}}))))

(deftest compose-test
  (is (= {:inserts {:a 2}}
         (thm/compose {:inserts {:a 1}}
                      {:updates {:a [1 2]}})))
  (is (= {:updates {:a [1 3]}}
         (thm/compose {:updates {:a [1 2]}}
                      {:updates {:a [2 3]}})))
  (is (= {:deletes {:a 1 :b 1}}
         (thm/compose {:deletes {:a 1}}
                      {:deletes {:b 1}})))
  (is (= {:inserts {:baz 4}
          :updates {:foo [2 3]}
          :deletes {:bar 5}}
         (thm/compose {:inserts {:baz 1} :updates {:foo [2 3]}}
                      {:updates {:baz [1 4]} :deletes {:bar 5}}))))

#_(deftest transform-test
  (is (= [{:inserts {:foo 1}} {:inserts {:bar 2}}]
         (thm/transform {:inserts {:foo 1}} {:inserts {:bar 2}})))
  (is (= [{:updates {:foo [1 2]}} {}]
         (thm/transform {:updates {:foo [1 2]}} {:deletes {:foo 1}})))
  (is (= [{} {}]
         (thm/transform {:deletes {:foo 1}} {:deletes {:foo 1}})))
  (is (= [{:deletes {:foo 1}} {:inserts {}}]
         (thm/transform {:deletes {:foo 1}} {:inserts {:foo 1}})))
  (is (= [{:inserts {:foo 1}} {}]
         (thm/transform {:inserts {:foo 1}} {:deletes {:foo 1}})))
  (is (= [{:inserts {:foo 1}} {}]
         (thm/transform {:inserts {:foo 1}} {:inserts {:foo 2}})))
  (is (= [{:updates {:foo [3 2]}} {}]
         (thm/transform {:updates {:foo [1 2]}} {:updates {:foo [1 3]}}))))
