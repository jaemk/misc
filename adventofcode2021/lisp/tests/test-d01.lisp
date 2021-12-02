(in-package :advent/tests)

(def-suite* test-d01 :in all)

(defparameter *test-input-day01-01*
  '(199
    200
    208
    210
    200
    207
    240
    269
    260
    263))

(test test-day01-part1-01
  (is (= 7 (advent.d01:part-1 *test-input-day01-01*))))

(test test-day01-part1-real
  (is (= 1548 (advent.d01:part-1 (advent.d01:input)))))

(test test-day01-part2-01
  (is (= 5 (advent.d01:part-2 *test-input-day01-01*))))

(test test-day01-part2-real
  (is (= 1589 (advent.d01:part-2 (advent.d01:input)))))

