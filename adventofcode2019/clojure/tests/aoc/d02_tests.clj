(ns aoc.d02-tests
  (:require [aoc.d02]
            [clojure.test :refer :all]))

(deftest p1-1
  (is (= 2 (first (aoc.d02/run-to-complete [1 0 0 0 99]))))
  (is (= 2 (first (aoc.d02/run-to-complete [2 3 0 3 99]))))
  (is (= 2 (first (aoc.d02/run-to-complete [2 4 4 5 99 0]))))
  (is (= 30 (first (aoc.d02/run-to-complete [1 1 1 4 99 5 6 0 99]))))
  )

