(ns transom.core-test
  (:require [clojure.test :refer :all]
            [transom.core :refer :all]))

(deftest count-op-test
  (let [op '([:= 2] [:- 1] [:+ "foo"])]
    (is (= (count-before op) 3))
    (is (= (count-after op) 5))))

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

(deftest align-test
  (is (= (align '([:= 2] [:+ "xy"] [:= 2])
                '([:= 1] [:- 2] [:= 1]))
         [[[:= 1] [:= 1]]
          [[:= 1] [:- 1]]
          [[:+ "xy"] :nop]
          [[:= 1] [:- 1]]
          [[:= 1] [:= 1]]]))
  (is (= (align '([:= 2] [:+ "x"] [:- 2])
                '([:= 2] [:+ "y"] [:- 2]))
         [[[:= 2]  [:= 2]]
          [[:+ "x"] :nop]
          [:nop [:+ "y"]]
          [[:- 2]  [:- 2]]]))
  (is (= (align '([:+ "x"] [:= 3])
                '([:= 3] [:+ "y"]))
         [[[:+ "x"] :nop]
          [[:= 3] [:= 3]]
          [:nop [:+ "y"]]])))


(defn transform-helper
  [in op1 op2]
  (let [[op1' op2'] (transform op1 op2)]
    (is (= (apply-ops in op1 op2')
           (apply-ops in op2 op1')))))

(deftest transform-test
  (transform-helper "food" '([:= 3] [:= 1]) '([:= 1] [:= 3]))
  (transform-helper "foo" '([:= 2] [:- 1]) '([:- 1] [:= 2]))
  (transform-helper "grandpa" '([:- 2] [:= 5]) '([:= 5] [:- 2]))
  (transform-helper "brian" '([:= 2] [:- 2] [:= 1]) '([:= 2] [:- 3]))
  (transform-helper "fuck" '([:= 2] [:+ "foo"] [:= 2]) '([:+ "bar"] [:= 4]))
  (transform-helper "pasta" '([:= 1] [:+ "izz"] [:- 3] [:= 1]) '([:- 1] [:= 4])))

(defn compose-helper 
  [in op1 op2]
  (let [composed-op (compose op1 op2)]
    (is (= (apply-ops in op1 op2)
           (apply-op in composed-op)))))
