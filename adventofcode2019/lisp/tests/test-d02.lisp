(in-package :advent19/tests)

(def-suite* test-d02 :in all)

(test test-day02-part1-01
  (is (equalp #(1 2 0 1 99) (advent19.d02:exec #(1 0 0 1 99)))))


