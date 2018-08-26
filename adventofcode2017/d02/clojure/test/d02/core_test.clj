(ns d02.core-test
  (:require [clojure.test :refer :all]
            [d02.core :refer :all]))

(deftest p-1
  (testing "part1"
    (let [in (str "5 1 9 5\n"
                  "7 5 3\n"
                  "2 4 6 8")
          in (parse-input in)]
      (is (= 18 (part-1 in))))))


(deftest p-2
  (testing "part2"
    (let [in (str "5 9 2 8\n"
                  "9 4 7 3\n"
                  "3 8 6 5")
          in (parse-input in)]
      (is (= 9 (part-2 in))))))
