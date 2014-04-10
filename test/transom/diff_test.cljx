(ns transom.diff-test
  #+clj
  (:require [clojure.test :refer :all]
            [transom.diff :refer :all]))

(deftest diff-test
  (are [x y] (= x y)
    (diff "foo" "bar") [[:- 3] [:+ "bar"]]
    (diff "foo" "food") [[:= 3] [:+ "d"]]
    (diff "ood" "good") [[:+ "g"] [:= 3]]
    (diff "good" "ood") [[:- 1] [:= 3]]
    (diff "pizza" "pa") [[:= 1] [:- 3] [:= 1]]
    (diff "asdf" "asf") [[:= 2] [:- 1] [:= 1]]
    (diff "brian" "brain") [[:= 2] [:- 2] [:+ "ai"] [:= 1]]
    (diff "moo" "mooo") [[:= 3] [:+ "o"]]
    (diff "oo" "ooo") [[:= 2] [:+ "o"]]
    (diff "ooo" "oo") [[:= 2] [:- 1]]))
