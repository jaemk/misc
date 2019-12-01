(ns aoc.d01-tests
  (:require [aoc.d01]
            [clojure.test :refer :all]))

(deftest p1-1
  (is (= 2 (aoc.d01/part1 ["12"])))
  (is (= 2 (aoc.d01/part1 ["14"])))
  (is (= 654 (aoc.d01/part1 ["1969"])))
  (is (= 33583 (aoc.d01/part1 ["100756"])))
  (is (= -1 (aoc.d01/part1 ["3"])))
  (is (= 7 (aoc.d01/part1 ["12" "24" "3"])))
  )

(deftest p1-2
  (is (= 2 (aoc.d01/part2 ["14"])))
  (is (= 966 (aoc.d01/part2 ["1969"])))
  (is (= 50346 (aoc.d01/part2 ["100756"])))
  )
