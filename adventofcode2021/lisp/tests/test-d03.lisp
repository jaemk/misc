(in-package :advent/tests)

(def-suite* test-d03 :in all)

(defparameter *test-input-day03-01*
  (vector
    (vector 0 0 1 0 0)
    (vector 1 1 1 1 0)
    (vector 1 0 1 1 0)
    (vector 1 0 1 1 1)
    (vector 1 0 1 0 1)
    (vector 0 1 1 1 1)
    (vector 0 0 1 1 1)
    (vector 1 1 1 0 0)
    (vector 1 0 0 0 0)
    (vector 1 1 0 0 1)
    (vector 0 0 0 1 0)
    (vector 0 1 0 1 0)))

(test test-day03-part1-01
  (is (= 198 (advent.d03:part-1 *test-input-day03-01*))))

(test test-day03-part1-real
  (is (= 1082324 (advent.d03:part-1 (advent.d03:input)))))

(test test-day03-part2-01
  (is (= 230 (advent.d03:part-2 *test-input-day03-01*))))

(test test-day03-part2-real
  (is (= 1353024 (advent.d03:part-2 (advent.d03:input)))))


