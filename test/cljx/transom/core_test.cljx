(ns transom.core-test
  #+clj
  (:require [clojure.test :refer :all]
            [transom.core :refer :all]))

(deftest count-test
  (let [edit [[:= 2] [:- 1] [:+ "foo"]]]
    (are [x y] (= x y)
      3 (count-before edit)
      5 (count-after  edit))))

(deftest patch-test
  (are [x y] (= x y)
    "abxcd"
    (patch "abcd" [[:= 2] [:+ "x"] [:= 2]])
    ""
    (patch "abcd" [[:- 4]])
    "axd"
    (patch "abxcd" [[:= 1] [:- 1] [:= 1] [:- 1] [:= 1]])
    "foobar"
    (patch "foobar" [[:= 3] [:+ "xs"] [:= 3]] [[:= 3] [:- 2] [:= 3]]))
  (is (thrown? AssertionError (patch "abcd" [[:= 2] [:+ "f"] [:- 1]]))))

(deftest pack-test
  (are [x y] (= x y)
    [[:= 5] [:+ "xsy"] [:= 3]]
    (pack [[:= 2] :nop [:= 3] [:+ "xs"] [:+ "y"] [:= 1] [:= 2]])
    [[:= 3] [:+ "xsy"] [:= 2]]
    (pack [[:= 2] [:= 1] [:= 0] :nop [:+ "xs"] :nop [:+ "y"] [:= 0] [:= 2]])))

(deftest align-transform-test
  (is (= (align-transform [[:= 2] [:+ "xy"] [:= 2]] [[:= 1] [:- 2] [:+ "z"] [:= 1]])
         [[[:= 1] [:= 1]]
          [[:= 1] [:- 1]]
          [[:+ "xy"] :nop]
          [[:= 1] [:- 1]]
          [:nop [:+ "z"]]
          [[:= 1] [:= 1]]])))

(defn transform-helper
  [in op1 op2]
  (let [[op1' op2'] (transform op1 op2)]
    (is (= (patch in op1 op2') (patch in op2 op1')))))

(deftest transform-test
  (transform-helper "food" [[:= 3] [:= 1]] [[:= 1] [:= 3]])
  (transform-helper "foo" [[:= 2] [:- 1]] [[:- 1] [:= 2]])
  (transform-helper "grandpa" [[:- 2] [:= 5]] [[:= 5] [:- 2]])
  (transform-helper "brian" [[:= 2] [:- 2] [:= 1]] [[:= 2] [:- 3]])
  (transform-helper "fuck" [[:= 2] [:+ "foo"] [:= 2]] [[:+ "bar"] [:= 4]])
  (transform-helper "pasta" [[:= 1] [:+ "izz"] [:- 3] [:= 1]] [[:- 1] [:= 4]]))

(deftest align-compose-test
  (are [x y] (= x y)
    (align-compose [[:+ "foo"] [:= 5]]
                   [[:= 5] [:- 3]])
      [[[:+ "foo"] [:= 3]]
       [[:= 2] [:= 2]]
       [[:= 3] [:- 3]]]
    (align-compose [[:+ "foo"] [:= 5]]
                   [[:= 2] [:- 6]])
      [[[:+ "fo"] [:= 2]]
       [[:+ "o"] [:- 1]]
       [[:= 5] [:- 5]]]
    (align-compose [[:- 2] [:+ "bar"] [:= 3]]
                   [[:+ "foo"] [:- 5] [:= 1]])
      [[[:- 2] :nop]
       [:nop [:+ "foo"]]
       [[:+ "bar"] [:- 3]]
       [[:= 2] [:- 2]]
       [[:= 1] [:= 1]]]))

(defn compose-helper 
  [in edit1 edit2]
  (let [composed-edit (compose edit1 edit2)]
    (is (= (patch in edit1 edit2)
           (patch in composed-edit)))))

(deftest compose-test
  (is (= (compose [[:+ "foo"] [:= 3] [:- 2]]
                  [[:= 3] [:+ "bar"] [:- 2] [:= 1]]))
      [[:+ "foo"] [:+ "bar"] [:- 2] [:- 2] [:= 1]]))

(deftest transform-caret-test
  (are [x y] (= x y)
    8 (transform-caret 5 [[:+ "foo"] [:= 6]])
    2 (transform-caret 5 [[:- 3] [:= 10]])
    5 (transform-caret 5 [[:= 5] [:+ "foo"]])))
