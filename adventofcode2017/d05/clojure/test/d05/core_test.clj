(ns d05.core-test
  (:require [clojure.test :refer :all]
            [d05.core :refer :all]))

(deftest test-part-1
  (testing "part 1"
    (is (= 5 (part-1 [0 3 0 1 -3])))))

(deftest test-part-2
  (testing "part 2"
    (is (= 10 (part-2 [0 3 0 1 -3])))))
