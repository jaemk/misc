(ns d03.core-test
  (:require [clojure.test :refer :all]
            [d03.core :as d03]))

(deftest p1
  (testing "part 1"
    (is (= 0 (d03/part-1 1))))
  (testing "part 1"
    (is (= 3 (d03/part-1 12))))
  (testing "part 1"
    (is (= 2 (d03/part-1 23))))
  (testing "part 1"
    (is (= 31 (d03/part-1 1024)))))

(deftest p2
  (testing "part 2"
    (is (= 54 (d03/part-2 26)))))
