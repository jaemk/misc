(in-package :advent/tests)

(def-suite* test-d11 :in all)

(defparameter *test-input-day11-01*
  "
11111
19991
19191
19991
11111
  ")

(defparameter *test-input-day11-02*
  "
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
  ")

(test test-day11-part1-01
  (is (= 9 (advent.d11:part-1 (advent.d11:parse *test-input-day11-01*) :steps 1))))

(test test-day11-part1-02
  (is (= 9 (advent.d11:part-1 (advent.d11:parse *test-input-day11-01*) :steps 2))))

(test test-day11-part1-03
  (is (= 0 (advent.d11:part-1 (advent.d11:parse *test-input-day11-02*) :steps 1))))

(test test-day11-part1-04
  (is (= 35 (advent.d11:part-1 (advent.d11:parse *test-input-day11-02*) :steps 2))))

(test test-day11-part1-05
  (is (= 204 (advent.d11:part-1 (advent.d11:parse *test-input-day11-02*) :steps 10))))

(test test-day11-part1-06
  (is (= 1656 (advent.d11:part-1 (advent.d11:parse *test-input-day11-02*)))))

(test test-day11-part1-real
  (is (= 1613 (advent.d11:part-1 (advent.d11:input)))))

(test test-day11-part2-01
  (is (= 195 (advent.d11:part-2 (advent.d11:parse *test-input-day11-02*)))))

(test test-day11-part2-real
  (is (= 510 (advent.d11:part-2 (advent.d11:input)))))

