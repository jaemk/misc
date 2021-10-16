(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d04 :in all)

(test test-day04-part1-real
  (is (= 579
    (advent19.d04:part-1
      (advent19.d04:input)))))

