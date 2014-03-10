(ns transom.core-test
  (:require [clojure.test :refer :all]
            [transom.core :refer :all]))

(deftest apply-op-test
  (is (= (apply-op "abcd" '([:= 2] [:+ "xs"] [:= 2]))
         "abxscd"))
  (is (= (apply-op "abcd" '([:- 4]))
         ""))
  (is (= (apply-op "abxcd" '([:= 1] [:- 1] [:= 1] [:- 1] [:= 1]))
         "axd"))
  (is (thrown? AssertionError (apply-op "abcd" '([:= 2] [:+ "f"] [:- 1])))))

(deftest pack-test
  (is (= (pack '([:= 2] [:= 3] [:+ "xs"] [:+ "y"] [:= 1] [:= 2]))
         '([:= 5] [:+ "xsy"] [:= 3])))
  (is (= (pack '([:= 2] [:= 1] [:= 0] [:+ "xs"] [:+ "y"] [:= 0] [:= 2]))
         '([:= 3] [:+ "xsy"] [:= 2]))))

(deftest transform-test
  (is (= (transform ['([:= 5]) '([:= 5])])
         ['([:= 5]) '([:= 5])])))
