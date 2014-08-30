(ns transom.map-test
  (:require [transom.map :as tm]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest diff-test
  (is (= {} (tm/diff {:a 1 :b 2 :c 3} {:a 1 :b 2 :c 3})))
  (is (= {:a [:delete 1] :b [:update 2 3] :c [:insert 4]}
         (tm/diff {:a 1 :b 2} {:b 3 :c 4}))))

(deftest patch-test
  (is (= {:b 3 :c 4}
         (tm/patch {:a 1 :b 2}
                    {:a [:delete 1]
                     :b [:update 2 3]
                     :c [:insert 4]}))))

(deftest compose-test
  (is (= {:a [:insert 2]}
         (tm/compose {:a [:insert 1]} {:a [:update 1 2]})))
  (is (= {}
         (tm/compose {:a [:insert 1]} {:a [:delete 1]})))
  (is (= {:a [:update 1 3]}
         (tm/compose {:a [:update 1 2]} {:a [:update 2 3]})))
  (is (= {:a [:delete 1]}
         (tm/compose {:a [:update 1 2]} {:a [:delete 2]})))
  (is (= {:a [:update 1 2]}
         (tm/compose {:a [:delete 1]} {:a [:insert 2]})))
  (is (= {:a [:update 1 5]
          :b [:delete 2]
          :c [:insert 6]
          :d [:insert 7]
          :e [:update 8 9]
          :f [:delete 10]}
         (tm/compose {:a [:delete 1]
                      :b [:update 2 3]
                      :c [:insert 4]}
                     {:a [:insert 5]
                      :b [:delete 3]
                      :c [:update 4 6]
                      :d [:insert 7]
                      :e [:update 8 9]
                      :f [:delete 10]}))))

(deftest transform-test
  (is (= [{} {}]
         (tm/transform {} {})))
  (is (= [{} {}]
         (tm/transform {:a [:insert 1]} {:a [:insert 1]})))
  (is (= [{:a [:update 2 1]} {}]
         (tm/transform {:a [:insert 1]} {:a [:insert 2]})))
  (is (= [{:a [:insert 1]} {:b [:insert 2]}]
         (tm/transform {:a [:insert 1]} {:b [:insert 2]})))
  (is (= [{} {}]
         (tm/transform {:a [:update 1 2]} {:a [:update 1 2]}))) 
  (is (= [{:a [:update 3 2]} {}]
         (tm/transform {:a [:update 1 2]} {:a [:update 1 3]})))
  (is (= [{} {:a [:delete 2]}]
         (tm/transform {:a [:update 1 2]} {:a [:delete 1]})))
  (is (= [{:a [:delete 2]} {}]
         (tm/transform {:a [:delete 1]} {:a [:update 1 2]})))
  (is (= [{} {}]
         (tm/transform {:a [:delete 1]} {:a [:delete 1]}))))

(def simple-map-gen (gen/map gen/keyword gen/int))

(defspec patching-diffs
  100
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen]
    (and (= m2 (tm/patch m1 (tm/diff m1 m2)))
         (= m1 (tm/patch m2 (tm/diff m2 m1))))))

(defspec composing-edits
  100
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen
                 m3 simple-map-gen]
    (let [edit1 (tm/diff m1 m2)
          edit2 (tm/diff m2 m3)]
      (= m3
         (tm/patch m1 edit1 edit2)
         (tm/patch m1 (tm/compose edit1 edit2))))))

(defspec transforming-edits
  100
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen
                 m3 simple-map-gen]
    (let [edit1 (tm/diff m1 m2)
          edit2 (tm/diff m1 m3)
          [edit1' edit2'] (tm/transform edit1 edit2)]
      (= (tm/patch m1 edit1 edit2')
         (tm/patch m1 edit2 edit1')))))

(defspec composing-transforms
  100
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen
                 m3 simple-map-gen]
    (let [old (tm/diff m1 m2)
          new (tm/diff m1 m3)
          [old' new'] (tm/transform old new)]
      (= (tm/compose old new') (tm/compose new old')))))

(defspec inverting-edits
  100
  (prop/for-all [m1 simple-map-gen
                 m2 simple-map-gen]
    (let [edit (tm/diff m1 m2)
          inverted (tm/invert edit)]
      (= m1 (tm/patch m1 edit inverted)))))
