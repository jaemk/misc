(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d03 :in all)

(test test-day03-part1-01
  (is (= 159
    (advent19.d03:part-1
      (advent19.d03:parse
        #?|R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83|)))))

(test test-day03-part1-02
  (is (= 135
    (advent19.d03:part-1
      (advent19.d03:parse
        #?|R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7|)))))

(test test-day03-part1-real
  (is (= 2193
    (advent19.d03:part-1
      (advent19.d03:input)))))

(test test-day03-part2-01
  (is (= 610
    (advent19.d03:part-2
      (advent19.d03:parse
        #?|R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83|)))))

(test test-day03-part2-02
  (is (= 410
    (advent19.d03:part-2
      (advent19.d03:parse
        #?|R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7|)))))

(test test-day03-part2-real
  (is (= 63526
    (advent19.d03:part-2
      (advent19.d03:input)))))

