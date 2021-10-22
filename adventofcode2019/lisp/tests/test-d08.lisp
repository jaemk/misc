(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d08 :in all)

(test test-day08-parse-01
  (is (equal
        (list (list 1 2 3 4 5 6) (list 7 8 9 0 1 2))
        (advent19.d08:parse "123456789012" :size '(3 2)))))


(test test-day08-part1-01
  (is (= 1 (advent19.d08:part-1 (advent19.d08:parse "123456789012" :size '(3 2))))))

(test test-day08-part1-real
  (is (= 1224 (advent19.d08:part-1 (advent19.d08:input)))))

(test test-day08-part2-01
  (is (equal (list 0 1 1 0) (advent19.d08:part-2
                              (advent19.d08:parse "0222112222120000" :size '(2 2))
                              :display nil))))

(test test-day08-part2-01
  (is (equal #?"\n X\nX " (advent19.d08:part-2
                              (advent19.d08:parse "0222112222120000" :size '(2 2))
                              :display '(2 2)))))

(test test-day08-part2-real
  (is (equal "
XXXX XXX  XXXX X  X XXX  
X    X  X    X X  X X  X 
XXX  XXX    X  X  X X  X 
X    X  X  X   X  X XXX  
X    X  X X    X  X X X  
XXXX XXX  XXXX  XX  X  X "
             (advent19.d08:part-2 (advent19.d08:input)))))

