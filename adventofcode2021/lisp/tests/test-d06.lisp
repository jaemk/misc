(in-package :advent/tests)

(def-suite* test-d06 :in all)

(defparameter *test-input-day06-01*
  (list 3 4 3 1 2))

(test test-day06-part1-01
  (is (= 5934 (advent.d06:part-1 *test-input-day06-01*))))

(test test-day06-part1-real
  (is (= 388419 (advent.d06:part-1 (advent.d06:input)))))

(test test-day06-part2-01
  (is (= 26 (advent.d06:part-2 *test-input-day06-01* :days 18))))

(test test-day06-part2-02
  (is (= 5934 (advent.d06:part-2 *test-input-day06-01* :days 80))))

(test test-day06-part2-03
  (is (= 26984457539 (advent.d06:part-2 *test-input-day06-01*))))

(test test-day06-part2-real
  (is (= 1740449478328 (advent.d06:part-2 (advent.d06:input)))))

