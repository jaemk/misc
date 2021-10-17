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

(test test-day04-part2-01
  (is (advent19.d04:valid-b? 112233)))

(test test-day04-part2-02
  (is (not (advent19.d04:valid-b? 123444))))

(test test-day04-part2-03
  (is (advent19.d04:valid-b? 111122)))

(test test-day04-part2-04
  (is (not (advent19.d04:valid-b? 353097))))

(test test-day04-part2-05
  (is (advent19.d04:valid-b? 355677)))

(test test-day04-part2-06
  (is (not (advent19.d04:valid-b? 356667))))

(test test-day04-part2-07
  (is (advent19.d04:valid-b? 335555)))

(test test-day04-part2-08
  (is (not (advent19.d04:valid-b? 356600))))

(test test-day04-part2-09
  (is (not (advent19.d04:valid-b? 305555))))

(test test-day04-part2-10
  (is (not (advent19.d04:valid-b? 455555))))

(test test-day04-part2-11
  (is (not (advent19.d04:valid-b? 455556))))

(test test-day04-part2-12
  (is (advent19.d04:valid-b? 155566)))

(test test-day04-part2-13
  (is (advent19.d04:valid-b? 111223)))

(test test-day04-part2-real
  (is (= 358
    (advent19.d04:part-2
      (advent19.d04:input)))))
