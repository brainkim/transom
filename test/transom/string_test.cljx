(ns transom.string-test
  #+clj
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]  
            [transom.string :refer :all]))

(deftest patch-test
  (are [x y] (= x y)
    "abxcd"
    (patch "abcd" [[:retain 2] [:insert "x"] [:retain 2]])
    ""
    (patch "abcd" [[:delete 4]])
    "axd"
    (patch "abxcd" [[:retain 1] [:delete 1] [:retain 1] [:delete 1] [:retain 1]])
    "foobar"
    (patch "foobar" [[:retain 3] [:insert "xs"] [:retain 3]] [[:retain 3] [:delete 2] [:retain 3]]))
  (is (thrown? AssertionError (patch "abcd" [[:retain 2] [:insert "f"] [:delete 1]]))))

(deftest pack-test
  (are [x y] (= x y)
    [[:retain 5] [:insert "xsy"] [:retain 3]]
    (pack [[:retain 2] nil [:retain 3] [:insert "xs"] [:insert "y"] [:retain 1] [:retain 2]])
    [[:retain 3] [:insert "xsy"] [:retain 2]]
    (pack [[:retain 2] [:retain 1] [:retain 0] nil [:insert "xs"] nil [:insert "y"] [:retain 0] [:retain 2]])))

(defn transform-helper
  [in op1 op2]
  (let [[op1' op2'] (transform op1 op2)]
    (is (= (patch in op1 op2') (patch in op2 op1')))))

(deftest transform-test
  (transform-helper "food"
                    [[:retain 3] [:retain 1]]
                    [[:retain 1] [:retain 3]])
  (transform-helper "foo"
                    [[:retain 2] [:delete 1]]
                    [[:delete 1] [:retain 2]])
  (transform-helper "grandpa"
                    [[:delete 2] [:retain 5]]
                    [[:retain 5] [:delete 2]])
  (transform-helper "brian"
                    [[:retain 2] [:delete 2] [:retain 1]]
                    [[:retain 2] [:delete 3]])
  (transform-helper "fuck"
                    [[:retain 2] [:insert "foo"][:retain 2]]
                    [[:insert "bar"] [:retain 4]])
  (transform-helper "pasta"
                    [[:retain 1] [:insert "izz"] [:delete 3] [:retain 1]]
                    [[:delete 1] [:retain 4]]))

(deftest compose-test
  (is (= (compose [[:insert "foo"] [:retain 3] [:delete 2]]
                  [[:retain 3] [:insert "bar"] [:delete 2] [:retain 1]]))
      [[:insert "foo"] [:insert "bar"] [:delete 2] [:delete 2] [:retain 1]]))

(deftest transform-caret-test
  (are [x y] (= x y)
    8 (transform-caret 5 [[:insert "foo"] [:retain 6]])
    2 (transform-caret 5 [[:delete 3] [:retain 10]])
    8 (transform-caret 5 [[:retain 5] [:insert "foo"]])
    4 (transform-caret 5 [[:retain 4] [:delete 1]])
    5 (transform-caret 8 [[:retain 4] [:delete 4] [:insert "m"] [:retain 4]])))

(deftest diff-test
  (are [x y] (= x y)
    (diff "foo" "bar") [[:delete 3] [:insert "bar"]]
    (diff "foo" "food") [[:retain 3] [:insert "d"]]
    (diff "ood" "good") [[:insert "g"] [:retain 3]]
    (diff "good" "ood") [[:delete 1] [:retain 3]]
    (diff "pizza" "pa") [[:retain 1] [:delete 3] [:retain 1]]
    (diff "asdf" "asf") [[:retain 2] [:delete 1] [:retain 1]]
    (diff "brian" "brain") [[:retain 2] [:delete 2] [:insert "ai"] [:retain 1]]
    (diff "moo" "mooo") [[:retain 3] [:insert "o"]]
    (diff "oo" "ooo") [[:retain 2] [:insert "o"]]
    (diff "ooo" "oo") [[:retain 2] [:delete 1]]))

(defspec patching-diffs
  1000
  (prop/for-all [s1 gen/string-ascii
                 s2 gen/string-ascii]
    (let [diff1 (diff s1 s2)
          diff2 (diff s2 s1)]
      (and (= (patch s1 diff1) s2)
           (= (patch s2 diff2) s1)))))

(defspec composing-edits
  1000
  (prop/for-all [s1 gen/string-ascii
                 s2 gen/string-ascii
                 s3 gen/string-ascii]
    (let [edit1 (diff s1 s2)
          edit2 (diff s2 s3)]
      (= s3
         (patch s1 edit1 edit2)
         (patch s1 (compose edit1 edit2))))))

(defspec transforming-edits
  1000
  (prop/for-all [s1 gen/string-ascii
                 s2 gen/string-ascii
                 s3 gen/string-ascii]
    (let [edit1 (diff s1 s2)
          edit2 (diff s1 s3)
          [edit1' edit2'] (transform edit1 edit2)]
      (= (patch s1 edit1 edit2')
         (patch s1 edit2 edit1')))))
