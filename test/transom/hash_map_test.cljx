(ns transom.hash-map-test
  (:require [transom.hash-map :as thm]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest diff-test
  (is (= {} (thm/diff {:a 1 :b 2 :c 3} {:a 1 :b 2 :c 3})))
  (is (= {:a [:delete 1] :b [:update 2 3] :c [:insert 4]}
         (thm/diff {:a 1 :b 2} {:b 3 :c 4}))))

(deftest patch-test
  (is (= {:b 3 :c 4}
         (thm/patch {:a 1 :b 2}
                    {:a [:delete 1]
                     :b [:update 2 3]
                     :c [:insert 4]}))))

(deftest compose-test
  (is (= {:a [:insert 2]}
         (thm/compose {:a [:insert 1]} {:a [:update 1 2]})))
  (is (= {}
         (thm/compose {:a [:insert 1]} {:a [:delete 1]})))
  (is (= {:a [:update 1 3]}
         (thm/compose {:a [:update 1 2]} {:a [:update 2 3]})))
  (is (= {:a [:delete 1]}
         (thm/compose {:a [:update 1 2]} {:a [:delete 2]})))
  (is (= {:a [:update 1 2]}
         (thm/compose {:a [:delete 1]} {:a [:insert 2]})))
  (is (= {:a [:update 1 5]
          :b [:delete 2]
          :c [:insert 6]
          :d [:insert 7]
          :e [:update 8 9]
          :f [:delete 10]}
         (thm/compose {:a [:delete 1] :b [:update 2 3] :c [:insert 4]}
                      {:a [:insert 5]
                       :b [:delete 3]
                       :c [:update 4 6]
                       :d [:insert 7]
                       :e [:update 8 9]
                       :f [:delete 10]}))))

(deftest transform-test
  (is (= [{} {}]
         (thm/transform {} {})))
  (is (= [{} {}]
         (thm/transform {:a [:insert 1]} {:a [:insert 1]})))
  (is (= [{:a [:update 2 1]} {}]
         (thm/transform {:a [:insert 1]} {:a [:insert 2]})))
  (is (= [{:a [:insert 1]} {:b [:insert 2]}]
         (thm/transform {:a [:insert 1]} {:b [:insert 2]})))
  (is (= [{} {}]
         (thm/transform {:a [:update 1 2]} {:a [:update 1 2]}))) 
  (is (= [{:a [:update 3 2]} {}]
         (thm/transform {:a [:update 1 2]} {:a [:update 1 3]})))
  (is (= [{:a [:insert 2]} {}]
         (thm/transform {:a [:update 1 2]} {:a [:delete 1]})))
  (is (= [{} {:a [:insert 2]}]
         (thm/transform {:a [:delete 1]} {:a [:update 1 2]})))
  (is (= [{} {}]
         (thm/transform {:a [:delete 1]} {:a [:delete 1]}))))

(def simple-map-gen (gen/map gen/keyword gen/int))

(defspec patching-diffs
  1000
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen]
    (and (= m2 (thm/patch m1 (thm/diff m1 m2)))
         (= m1 (thm/patch m2 (thm/diff m2 m1))))))

(defspec composing-edits
  1000
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen
                 m3 simple-map-gen]
    (let [edit1 (thm/diff m1 m2)
          edit2 (thm/diff m2 m3)]
      (= m3
         (thm/patch (thm/patch m1 edit1) edit2)
         (thm/patch m1 (thm/compose edit1 edit2))))))

(defspec transforming-edits
  1000
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen
                 m3 simple-map-gen]
    (let [edit1 (thm/diff m1 m2)
          edit2 (thm/diff m1 m3)
          [edit1' edit2'] (thm/transform edit1 edit2)]
      (= (thm/patch (thm/patch m1 edit1) edit2')
         (thm/patch (thm/patch m1 edit2) edit1')))))
