(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d06 :in all)

(defvar p1-input "
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(test test-day06-part1-01
  (bind ((in (advent19.d06:parse p1-input)))
    (is (= 42 (advent19.d06:part-1 in)))))

(test test-day06-part1-real
  (is (= 204521 (advent19.d06:part-1 (advent19.d06:input)))))

(defvar p2-input "
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(test test-day06-part2-parents-01
  (bind ((in (advent19.d06:parse p2-input)))
    (is (equal (list "COM" "B" "C") (advent19.d06:parents in "D")))))

(test test-day06-part2-parents-02
  (bind ((in (advent19.d06:parse p2-input)))
    (is (equal (list "COM" "B" "C" "D" "E") (advent19.d06:parents in "F")))))

(test test-day06-part2-parents-03
  (bind ((in (advent19.d06:parse p2-input)))
    (is (equal (list "COM" "B" "C" "D" "E" "J" "K") (advent19.d06:parents in "YOU")))))

(test test-day06-part2-parents-04
  (bind ((in (advent19.d06:parse p2-input)))
    (is (equal (list "COM" "B" "C" "D" "I") (advent19.d06:parents in "SAN")))))

(test test-day06-part2-nca-01
  (bind ((in (advent19.d06:parse p2-input)))
    (is (equal "D" (advent19.d06:nearest-common-ancestor in "YOU" "SAN")))))

(test test-day06-part2-01
  (bind ((in (advent19.d06:parse p2-input)))
    (is (= 4 (advent19.d06:part-2 in)))))

(test test-day06-part2-real
  (bind ((in (advent19.d06:input)))
    (is (= 307 (advent19.d06:part-2 in)))))

