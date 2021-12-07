(in-package :advent/tests)

(def-suite* test-d07 :in all)

(defparameter *test-input-day07-01*
  (list 16 1 2 0 4 2 7 1 2 14))

(test test-day07-part1-01
  (is (equal (list 2 37) (advent.d07:part-1 *test-input-day07-01*))))

(test test-day07-part1-real
  (is (equal (list 342 325528) (advent.d07:part-1 (advent.d07:input)))))

(test test-day07-part2-series-sum-01
  (is (= 900 (advent.d07:series-sum 1 2 30))))

(test test-day07-part2-series-sum-02
  (is (= 66 (advent.d07:series-sum 1 1 (- 16 5)))))

(test test-day07-part2-fuel-01
  (is (= 66 (advent.d07:fuel 16 5)))
  (is (= 66 (advent.d07:fuel 5 16))))

(test test-day07-part2-01
  (is (equal (list 5 168) (advent.d07:part-2 *test-input-day07-01*))))

(test test-day07-part2-real
  (is (equal (list 460 85015836) (advent.d07:part-2 (advent.d07:input)))))

