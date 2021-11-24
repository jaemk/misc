(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d13 :in all)

(test test-day13-part1-real
  (is (= 193
         (-> (advent19.d13:input)
             (advent19.d13:part-1)))))

(test test-day13-get-dims
  (is (equal
        (list 43 19)
        (-> (advent19.d13:input)
            (advent19.d13:get-dims)))))

