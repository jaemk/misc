(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d09 :in all)

(test test-day09-parse-op-01
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 99)))
    (is (eql 99 op))
    (is (eql :pos m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-02
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 1002)))
    (is (eql 2 op))
    (is (eql :pos m1))
    (is (eql :imd m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-03
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 1132)))
    (is (eql 32 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-04
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 11103)))
    (is (eql 3 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :imd m3))))

(test test-day05-parse-op-05
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 101)))
    (is (eql 1 op))
    (is (eql :imd m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day09-parse-op-06
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 299)))
    (is (eql 99 op))
    (is (eql :rel m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-07
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 2102)))
    (is (eql 2 op))
    (is (eql :imd m1))
    (is (eql :rel m2))
    (is (eql :pos m3))))

(test test-day05-parse-op-08
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 12132)))
    (is (eql 32 op))
    (is (eql :imd m1))
    (is (eql :rel m2))
    (is (eql :imd m3))))

(test test-day05-parse-op-09
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 21103)))
    (is (eql 3 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :rel m3))))

(test test-day05-parse-op-10
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 201)))
    (is (eql 1 op))
    (is (eql :rel m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

