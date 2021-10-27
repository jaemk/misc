(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d10 :in all)

(defvar sample-in-1 "
.#..#
.....
#####
....#
...##
")

(test test-day10-parse-01
  (is (equalp (vector
                (vector 0 1 0 0 1)
                (vector 0 0 0 0 0)
                (vector 1 1 1 1 1)
                (vector 0 0 0 0 1)
                (vector 0 0 0 1 1))
              (advent19.d10:parse sample-in-1))))


(test test-day10-grid->coords-01
  (is (equalp (list
                (list 1 0)
                (list 4 0)
                (list 0 2)
                (list 1 2)
                (list 2 2)
                (list 3 2)
                (list 4 2)
                (list 4 3)
                (list 3 4)
                (list 4 4))
              (-> sample-in-1
                (advent19.d10:parse)
                (advent19.d10:grid->coords)))))

(test test-day10-part1-01
  (is (equalp (list 8 (list 3 4))
              (-> sample-in-1
                (advent19.d10:parse)
                (advent19.d10:part-1)))))

(defvar sample-in-2 "
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
")

(test test-day10-part1-02
  (is (equalp (list 33 (list 5 8))
              (-> sample-in-2
                (advent19.d10:parse)
                (advent19.d10:part-1)))))

(defvar sample-in-3 "
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
")

(test test-day10-part1-03
  (is (equalp (list 35 (list 1 2))
              (-> sample-in-3
                (advent19.d10:parse)
                (advent19.d10:part-1)))))

(defvar sample-in-4 "
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
")

(test test-day10-part1-04
  (is (equalp (list 41 (list 6 3))
              (-> sample-in-4
                (advent19.d10:parse)
                (advent19.d10:part-1)))))

(defvar sample-in-5 "
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
")

(test test-day10-part1-05
  (is (equalp (list 210 (list 11 13))
              (-> sample-in-5
                (advent19.d10:parse)
                (advent19.d10:part-1)))))

(test test-day10-part1-real
  (is (equalp (list 286 (list 22 25))
              (-> (advent19.d10:input)
                (advent19.d10:part-1)))))

