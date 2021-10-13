(in-package :advent19/tests)

(def-suite* test-d01 :in all)

(test test-day01-part1-01
  (is (= 2 (advent19.d01:part-1 '(12)))))

(test test-day01-part1-02
  (is (= 2 (advent19.d01:part-1 '(14)))))

(test test-day01-part1-03
  (is (= 654 (advent19.d01:part-1 '(1969)))))

(test test-day01-part1-04
  (is (= 33583 (advent19.d01:part-1 '(100756)))))

(test test-day01-part1-real
  (is (= 3305041 (advent19.d01:part-1 (advent19.d01:input)))))

(test test-day01-part2-01
  (is (= 2 (advent19.d01:part-2 '(12)))))

(test test-day01-part2-02
  (is (= 2 (advent19.d01:part-2 '(14)))))

(test test-day01-part2-03
  (is (= 966 (advent19.d01:part-2 '(1969)))))

(test test-day01-part2-04
  (is (= 50346 (advent19.d01:part-2 '(100756)))))

(test test-day01-part2-real
  (is (= 4954710 (advent19.d01:part-2 (advent19.d01:input)))))

