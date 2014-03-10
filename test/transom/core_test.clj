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

(deftest apply-ops-test
  (is (= (apply-ops "foobar" '([:= 3] [:+ "xs"] [:= 3]) '([:= 3] [:- 2] [:= 3]))
         "foobar")))

(deftest pack-test
  (is (= (pack '([:= 2] [:= 3] [:+ "xs"] [:+ "y"] [:= 1] [:= 2]))
         '([:= 5] [:+ "xsy"] [:= 3])))
  (is (= (pack '([:= 2] [:= 1] [:= 0] [:+ "xs"] [:+ "y"] [:= 0] [:= 2]))
         '([:= 3] [:+ "xsy"] [:= 2]))))

(deftest transform-test
  (let [op1 '([:+ "x"] [:= 4])
        op2 '([:= 4] [:+ "x"])
        [op2' op1'] (transform [op1 op2])]
    (is (= (apply-ops "abcd" op1 op2')
           (apply-ops "abcd" op2 op1')))))
