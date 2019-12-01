(ns aoc.d02-tests
  (:require [aoc.d02]
            [clojure.test :refer :all]))

(deftest p2-1
  (is (= 2 (aoc.d02/part1 ["aabcdef" "aabbb"])))
  (is (= 0 (aoc.d02/part1 ["1abcdef" "bbb"])))
  (is (= 1 (aoc.d02/part1 ["abccd" "111"]))))

(deftest p2-2
  (is (= "123" (aoc.d02/part2 ["1234" "1235" "2222"])))
  (is (nil? (aoc.d02/part2 ["1111" "2222"]))))
