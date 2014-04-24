(ns transom.diff-test
  #+clj
  (:require [clojure.test :refer :all]
            [transom.diff :refer :all]))

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
