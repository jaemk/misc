(in-package :advent/tests)

(def-suite* test-d03 :in all)

(defparameter *test-input-day03-01*
  (list
    5
    (list
      (parse-integer "00100" :radix 2)
      (parse-integer "11110" :radix 2)
      (parse-integer "10110" :radix 2)
      (parse-integer "10111" :radix 2)
      (parse-integer "10101" :radix 2)
      (parse-integer "01111" :radix 2)
      (parse-integer "00111" :radix 2)
      (parse-integer "11100" :radix 2)
      (parse-integer "10000" :radix 2)
      (parse-integer "11001" :radix 2)
      (parse-integer "00010" :radix 2)
      (parse-integer "01010" :radix 2))))

(test test-day03-part1-01
  (is (= 198 (advent.d03:part-1 *test-input-day03-01*))))

(test test-day03-part1-real
  (is (= 1082324 (advent.d03:part-1 (advent.d03:input)))))

(test test-day03-part2-01
  (is (= 230 (advent.d03:part-2 *test-input-day03-01*))))

(test test-day03-part2-real
  (is (= 1353024 (advent.d03:part-2 (advent.d03:input)))))


