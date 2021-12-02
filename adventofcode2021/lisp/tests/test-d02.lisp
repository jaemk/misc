(in-package :advent/tests)

(def-suite* test-d02 :in all)

(test test-day02-parse-move
  (is (equal (list "up" 1)
             (advent.d02:parse-move " up 1 ")))
  (is (equal (list "down" 2)
             (advent.d02:parse-move " down 2 ")))
  (is (equal (list "forward" 3)
             (advent.d02:parse-move " forward 3 "))))


(defparameter *test-input-day02-01*
  (list
    (list "forward" 5)
    (list "down" 5)
    (list "forward" 8)
    (list "up" 3)
    (list "down" 8)
    (list "forward" 2)))

(test test-part1-01
  (is (= 150 (advent.d02:part-1 *test-input-day02-01*))))

(test test-day02-part1-real
  (is (= 1648020 (advent.d02:part-1 (advent.d02:input)))))

(test test-part2-01
  (is (= 900 (advent.d02:part-2 *test-input-day02-01*))))

(test test-day02-part2-real
  (is (= 1759818555 (advent.d02:part-2 (advent.d02:input)))))

