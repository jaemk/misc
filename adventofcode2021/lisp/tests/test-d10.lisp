(in-package :advent/tests)

(def-suite* test-d10 :in all)

(defparameter *test-input-day10-01*
  "
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
  ")


(test test-day10-part1-01
  (is (= 26397 (advent.d10:part-1 (advent.d10:parse *test-input-day10-01*)))))

(test test-day10-part1-real
  (is (= 392043 (advent.d10:part-1 (advent.d10:input)))))

(test test-day10-part2-01
  (is (= 288957 (advent.d10:part-2 (advent.d10:parse *test-input-day10-01*)))))

(test test-day10-part2-real
  (is (= 1605968119 (advent.d10:part-2 (advent.d10:input)))))

