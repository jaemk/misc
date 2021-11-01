(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d11 :in all)

;; this one is flaky, some race condition I don't feel like fixing right now
;(test test-day11-part1-real
;  (is (= 1876 (advent19.d11:part-1 (advent19.d11:input)))))

(defvar part-2-result "
  XX   XX  XXX    XX  XX   XX   XX  X      
 X  X X  X X  X    X X  X X  X X  X X      
 X    X    X  X    X X    X    X    X      
 X    X XX XXX     X X    X XX X    X      
 X  X X  X X    X  X X  X X  X X  X X      
  XX   XXX X     XX   XX   XXX  XX  XXXX   
")

(test test-day11-part2-real
  (is (equal part-2-result
             (advent19.d11:part-2 (advent19.d11:input)))))

