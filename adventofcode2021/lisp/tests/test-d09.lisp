(in-package :advent/tests)

(def-suite* test-d09 :in all)

(defparameter *test-input-day09-01*
  "
2199943210
3987894921
9856789892
8767896789
9899965678
  ")

(test test-day09-part1-01
  (is (= 15 (advent.d09:part-1 (advent.d09:parse *test-input-day09-01*)))))

(test test-day09-part1-real
  (is (= 486 (advent.d09:part-1 (advent.d09:input)))))

(test test-day09-part2-01
  (is (= 1134 (advent.d09:part-2 (advent.d09:parse *test-input-day09-01*)))))

(test test-day09-part2-real
  (is (= 1059300 (advent.d09:part-2 (advent.d09:input)))))

