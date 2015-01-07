(ns transom.string-test
  #+clj
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]  
            [transom.string :refer :all]))

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

(defspec inverting-edits
  1000
  (prop/for-all [m1 gen/string-ascii
                 m2 gen/string-ascii]
    (let [edit (diff m1 m2)
          inverted (invert edit)]
      (= m1 (patch m1 edit inverted)))))
