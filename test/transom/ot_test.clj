(ns transom.ot-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [transom.ot :refer :all]))

(fact "operations have correct before and after length"
  (let [edit [[:= 2] [:- 1] [:+ "foo"]]]
    (count-before edit) => 3
    (count-after edit)  => 5))

(fact "apply-edit"
  (apply-edit "abcd" [[:= 2] [:+ "xs"] [:= 2]]) => "abxscd"
  (apply-edit "abcd" [[:- 4]]) => ""
  (apply-edit "abxcd" [[:= 1] [:- 1] [:= 1] [:- 1] [:= 1]]) => "axd"
  (apply-edit "abcd" [[:= 2] [:+ "f"] [:- 1]]) => (throws AssertionError))

(fact "apply-edits"
  (apply-edits "foobar" [[:= 3] [:+ "xs"] [:= 3]] [[:= 3] [:- 2] [:= 3]])
    => "foobar")

(fact "pack packs operation"
  (pack [[:= 2] :nop [:= 3] [:+ "xs"] [:+ "y"] [:= 1] [:= 2]])
    => [[:= 5] [:+ "xsy"] [:= 3]]
  (pack [[:= 2] [:= 1] [:= 0] :nop [:+ "xs"] :nop [:+ "y"] [:= 0] [:= 2]])
    => [[:= 3] [:+ "xsy"] [:= 2]])

(fact "align-transform aligns two operations"
  (align-transform [[:= 2] [:+ "xy"] [:= 2]]
                   [[:= 1] [:- 2] [:= 1]])
    => [[[:= 1] [:= 1]]
        [[:= 1] [:- 1]]
        [[:+ "xy"] :nop]
        [[:= 1] [:- 1]]
        [[:= 1] [:= 1]]]
  (align-transform [[:= 2] [:+ "x"] [:- 2]]
                   [[:= 2] [:+ "y"] [:- 2]])
    => [[[:= 2]  [:= 2]]
        [[:+ "x"] :nop]
        [:nop [:+ "y"]]
        [[:- 2]  [:- 2]]] 
  (align-transform [[:+ "x"] [:= 3]]
                   [[:= 3] [:+ "y"]])
    => [[[:+ "x"] :nop]
        [[:= 3] [:= 3]]
        [:nop [:+ "y"]]])

(defn transform-helper
  [in op1 op2]
  (let [[op1' op2'] (transform op1 op2)]
    (is (= (apply-edits in op1 op2')
           (apply-edits in op2 op1')))))

(deftest transform-test
  (transform-helper "food" [[:= 3] [:= 1]]
                           [[:= 1] [:= 3]])
  (transform-helper "foo" [[:= 2] [:- 1]]
                          [[:- 1] [:= 2]])
  (transform-helper "grandpa" [[:- 2] [:= 5]]
                              [[:= 5] [:- 2]])
  (transform-helper "brian" [[:= 2] [:- 2] [:= 1]]
                            [[:= 2] [:- 3]])
  (transform-helper "fuck" [[:= 2] [:+ "foo"] [:= 2]]
                           [[:+ "bar"] [:= 4]])
  (transform-helper "pasta" [[:= 1] [:+ "izz"] [:- 3] [:= 1]]
                            [[:- 1] [:= 4]]))

(deftest align-compose-test
  (is (= (align-compose [[:+ "foo"] [:= 5]]
                        [[:= 5] [:- 3]])
         [[[:+ "foo"] [:= 3]]
          [[:= 2] [:= 2]]
          [[:= 3] [:- 3]]])) 

  (is (= (align-compose [[:+ "foo"] [:= 5]]
                        [[:= 2] [:- 6]])
         [[[:+ "fo"] [:= 2]]
          [[:+ "o"] [:- 1]]
          [[:= 5] [:- 5]]]))

  (is (= (align-compose [[:- 2] [:+ "bar"] [:= 3]]
                        [[:+ "foo"] [:- 5] [:= 1]])
         [[[:- 2] :nop]
          [:nop [:+ "foo"]]
          [[:+ "bar"] [:- 3]]
          [[:= 2] [:- 2]]
          [[:= 1] [:= 1]]]))

  (is (= (align-compose [[:+ "foo"] [:= 3] [:- 2]]
                        [[:= 3] [:+ "bar"] [:= 1] [:- 2]])
         [[[:+ "foo"] [:= 3]]
          [:nop [:+ "bar"]]
          [[:= 1] [:= 1]]
          [[:= 2] [:- 2]]
          [[:- 2] :nop]]))

  (is (= (align-compose [[:- 3] [:= 2]]
                        [[:+ "foo"] [:- 2]])
         [[[:- 3] :nop]
          [:nop [:+ "foo"]]
          [[:= 2] [:- 2]]])))

(defn compose-helper 
  [in edit1 edit2]
  (let [composed-edit (compose edit1 edit2)]
    (is (= (apply-edits in edit1 edit2)
           (apply-edit in composed-edit)))))

(deftest compose-test
  (is (= (compose [[:+ "foo"] [:= 3] [:- 2]]
                  [[:= 3] [:+ "bar"] [:- 2] [:= 1]]))
      [[:+ "foo"] [:+ "bar"] [:- 2] [:- 2] [:= 1]]))

(fact "carets are transformed against operations"
  (transform-caret 5 [[:+ "foo"] [:= 6]]) => 8
  (transform-caret 5 [[:- 3] [:= 10]]) => 2
  (transform-caret 5 [[:= 5] [:+ "foo"]]) => 5)
