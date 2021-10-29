(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d11 :in all)

(test test-day11-part1-real
  (is (= 1876 (advent19.d11:part-1 (advent19.d11:input)))))

