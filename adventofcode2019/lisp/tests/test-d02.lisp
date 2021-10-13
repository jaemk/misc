(in-package :advent19/tests)

(def-suite* test-d02 :in all)

(test test-day02-part1-01
  (is (equalp #(1 2 0 1 99) (advent19.d02:exec #(1 0 0 1 99)))))

(test test-day02-part1-02
  (is (equalp #(2 0 0 0 99) (advent19.d02:exec #(1 0 0 0 99)))))

(test test-day02-part1-03
  (is (equalp #(2 3 0 6 99) (advent19.d02:exec #(2 3 0 3 99)))))

(test test-day02-part1-04
  (is (equalp #(2 4 4 5 99 9801) (advent19.d02:exec #(2 4 4 5 99 0)))))

(test test-day02-part1-05
  (is (equalp #(30 1 1 4 2 5 6 0 99) (advent19.d02:exec #(1 1 1 4 99 5 6 0 99)))))

(test test-day02-part1-real
  (is (= 3706713 (advent19.d02:part-1 (advent19.d02:input)))))

