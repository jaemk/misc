(ns aoc.d01-tests
  (:require [aoc.d01]
            [clojure.test :refer :all]))

(deftest p1-1
  (is (= 2 (aoc.d01/part1 ["+1" "-1" "+2"])))
  (is (= -3 (aoc.d01/part1 ["-1" "-2"]))))

(deftest p1-2
  (is (= 1 (aoc.d01/part2 ["+1" "-1" "+1" "-2"])))
  (is (= 2 (aoc.d01/part2 ["+1" "+1" "+1" "-1"]))))
