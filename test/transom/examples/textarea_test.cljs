(ns transom.examples.textarea-test
  (:require-macros [cemerick.cljs.test :refer
                    [is deftest with-test run-tests testing test-var]])
  (:require [cemerick.cljs.test :as t]))

(deftest first-test
  (println "hello?")
  (is (= true false)))