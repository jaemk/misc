(in-package :advent/tests)

(def-suite* test-d05 :in all)

(defparameter *test-input-day05-01*
  "
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
  ")

(test test-day05-part1-01
  (is (= 5 (advent.d05:part-1 (advent.d05:parse *test-input-day05-01*)))))

(test test-day05-part1-real
  (is (= 8350 (advent.d05:part-1 (advent.d05:input)))))

(test test-day05-part2-01
  (is (= 12 (advent.d05:part-2 (advent.d05:parse *test-input-day05-01*)))))

(test test-day05-part2-real
  (is (= 19374 (advent.d05:part-2 (advent.d05:input)))))

