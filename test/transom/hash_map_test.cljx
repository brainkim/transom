(ns transom.hash-map-test
  (:require [transom.hash-map :as thm]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest diff-test
  (is (= {} (thm/diff {:a 1 :b 2 :c 3}
                  {:a 1 :b 2 :c 3})))
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

;; insert the same key -> left side wins unless same value then noop
;; insert different keys -> no change
;; (should updates always win over deletes?)
;; update vs delete the same key -> left side (update) wins
;; delete vs update the same key -> left side (delete) wins
;; delete the same key -> noop

(deftest transform-test
  (comment
  (is (= [{} {}]
         (thm/transform {} {})))
  (is (= [{} {}]
         (thm/transform {:inserts {:a 1}} {:inserts {:a 1}})))
  (is (= [{:updates {:a [2 1]}} {}]
         (thm/transform {:inserts {:a 1}} {:inserts {:a 2}})))
  (is (= [{:inserts {:a 1}} {:inserts {:b 2}}]
         (thm/transform {:inserts {:a 1}} {:inserts {:b 2}})))
  (is (= [{} {}]
         (thm/transform {:updates {:a [1 2]}} {:updates {:a [1 2]}}))) 
  (is (= [{:updates {:a [3 2]}} {}]
         (thm/transform {:updates {:a [1 2]}} {:updates {:a [1 3]}})))
  (is (= [{:inserts {:a 2}} {}]
         (thm/transform {:updates {:a [1 2]}} {:deletes {:a 1}})))
  (is (= [{} {:inserts {:a 2}}]
         (thm/transform {:deletes {:a 1}} {:updates {:a [1 2]}})))
  (is (= [{} {}]
         (thm/transform {:deletes {:a 1}} {:deletes {:a 1}})))
  (is (= []
         (thm/transform {:inserts {:a 1}
                         :deletes {:b 1}}
                        {:inserts {:a 2}
                         :updates {:b [1 2]}})))))
