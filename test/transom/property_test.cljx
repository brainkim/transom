(ns transom.property-test
  (:require [transom.core :as transom]
            [transom.diff :as diff]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defspec patching-diffs
  1000
  (prop/for-all [s1 gen/string-ascii
                 s2 gen/string-ascii]
    (let [diff1 (diff/diff s1 s2)
          diff2 (diff/diff s2 s1)]
      (and (= (transom/patch s1 diff1) s2)
           (= (transom/patch s2 diff2) s1)))))

;; TODO: Is is possible to write these property tests without diff?
(defspec composing-edits
  1000
  (prop/for-all [s1 gen/string-ascii
                 s2 gen/string-ascii
                 s3 gen/string-ascii]
    (let [edit1 (diff/diff s1 s2)
          edit2 (diff/diff s2 s3)]
      (= s3 (transom/patch s1 edit1 edit2) (transom/patch s1 (transom/compose edit1 edit2))))))

(defspec transforming-edits
  1000
  (prop/for-all [s1 gen/string-ascii
                 s2 gen/string-ascii
                 s3 gen/string-ascii]
    (let [edit1 (diff/diff s1 s2)
          edit2 (diff/diff s1 s3)
          [edit1' edit2'] (transom/transform edit1 edit2)]
      (= (transom/patch s1 edit1 edit2')
         (transom/patch s1 edit2 edit1')))))
