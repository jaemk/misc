(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d04 :in all)

(test test-day04-part1-01
  (is (advent19.d04:valid-a? 111111)))

(test test-day04-part1-02
  (is (not (advent19.d04:valid-a? 223450))))

(test test-day04-part1-03
  (is (not (advent19.d04:valid-a? 123789))))

(test test-day04-part1-real
  (is (= 579
    (advent19.d04:part-1
      (advent19.d04:input)))))

