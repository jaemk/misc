(ns d01.core-test
  (:require [clojure.test :refer :all]
            [d01.core :as d01]))

(deftest part1
  (testing "part1"
    (is (= 3
           (d01/part-1 (d01/parse-input "1122")))))
  (testing "part1"
    (is (= 0
           (d01/part-1 (d01/parse-input "1234")))))
  (testing "part1"
    (is (= 9
           (d01/part-1 (d01/parse-input "91212129")))))
  (testing "part1"
    (is (= 4
           (d01/part-1 (d01/parse-input "1111"))))))


(deftest part2
  (testing "part2"
    (is (= 6
           (d01/part-2 (d01/parse-input "1212")))))
  (testing "part2"
    (is (= 0
           (d01/part-2 (d01/parse-input "1221")))))
  (testing "part2"
    (is (= 4
           (d01/part-2 (d01/parse-input "123425")))))
  (testing "part2"
    (is (= 12
           (d01/part-2 (d01/parse-input "123123")))))
  (testing "part2"
    (is (= 4
           (d01/part-2 (d01/parse-input "12131415"))))))
