(ns keyfunc.core-test
  (:require [clojure.test :refer :all]
            [keyfunc.core :refer :all]))

(deftest basic-key
  (testing "Basic keyfn"
    (is (= '(9 4 5)
           (keyfn [3 4 5 6] [2 0 1 2] sum)))))

(deftest histo-key
  (testing "Histogram from keyfn"
    (is (= [[5 3]
            [4 2]
            [1 1]]
           (keyfn-histo [5 4 1 4 5 5])))))

(deftest sum-of-groups
  (testing "grouped sums"
    (is (= [[:a 3]
            [:b 1]]
           (grouped-sum [[:a 2] [:b 1] [:a 1]])))))

(deftest group-nub
  (testing "grouped nub"
    (is (= [[:a 10]
            [:b 3]]
           (grouped-nub [[:a 10] [:a 20] [:b 3]])))))

