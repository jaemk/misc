(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d09 :in all)

(test test-day09-parse-op-01
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 99)))
    (is (eql 99 op))
    (is (eql :pos m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day09-parse-op-02
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 1002)))
    (is (eql 2 op))
    (is (eql :pos m1))
    (is (eql :imd m2))
    (is (eql :pos m3))))

(test test-day09-parse-op-03
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 1132)))
    (is (eql 32 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :pos m3))))

(test test-day09-parse-op-04
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 11103)))
    (is (eql 3 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :imd m3))))

(test test-day09-parse-op-05
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

(test test-day09-parse-op-07
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 2102)))
    (is (eql 2 op))
    (is (eql :imd m1))
    (is (eql :rel m2))
    (is (eql :pos m3))))

(test test-day09-parse-op-08
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 12132)))
    (is (eql 32 op))
    (is (eql :imd m1))
    (is (eql :rel m2))
    (is (eql :imd m3))))

(test test-day09-parse-op-09
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 21103)))
    (is (eql 3 op))
    (is (eql :imd m1))
    (is (eql :imd m2))
    (is (eql :rel m3))))

(test test-day09-parse-op-10
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 201)))
    (is (eql 1 op))
    (is (eql :rel m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day09-parse-op-11
  (bind (((:values op m1 m2 m3) (advent19.vm:parse-op-code 109)))
    (is (eql 9 op))
    (is (eql :imd m1))
    (is (eql :pos m2))
    (is (eql :pos m3))))

(test test-day09-test-base-01
  (is (= 4
         (->
           (advent19.vm:run-vm-with #(109 4 99))
           (advent19.vm:vm-rel-base)))))

(test test-day09-test-base-02
  (is (= 210
         (->
           (advent19.vm:run-vm-with #(109 1 209 1 99))
           (advent19.vm:vm-rel-base)))))

(test test-day09-test-base-03
  (is (= 1985
         (->
           (advent19.vm:start-vm-with #(109 19 109 -34 99) :rel-base 2000)
           (advent19.vm:wait-vm)
           (advent19.vm:vm-rel-base)))))

(test test-day09-test-base-04
  (is (= 5
         (bind ((res nil))
           (->
             (advent19.vm:start-vm-with #(109 19 3 1985 204 -34 99)
                                        :rel-base 2000
                                        :write-fn (lambda (vmi val) (push val res)))
             (advent19.vm:send-vm 5)
             (advent19.vm:wait-vm))
           (first res)))))

(test test-day09-samples-01
  (is (equal (list 109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99)
             (bind ((res nil))
               (->
                 (advent19.vm:start-vm-with #(109 1 204 -1 1001 100 1 100 1008 100
                                              16 101 1006 101 0 99)
                                            :write-fn (lambda (vmi val) (push val res)))
                 (advent19.vm:wait-vm))
               (reverse res)))))

(test test-day09-samples-02
  (is (= 16
         (bind ((res nil))
           (->
             (advent19.vm:start-vm-with #(1102 34915192 34915192 7 4 7 99 0)
                                        :write-fn (lambda (vmi val) (push val res)))
             (advent19.vm:wait-vm))
           (->>
             (first res)
             (format nil "~a")
             (length))))))

(test test-day09-samples-03
  (is (= 1125899906842624
         (bind ((res nil))
           (->
             (advent19.vm:start-vm-with #(104 1125899906842624 99)
                                        :write-fn (lambda (vmi val) (push val res)))
             (advent19.vm:wait-vm))
           (first res)))))

(test test-day09-samples-04
  (is (= 2
         (bind ((res nil))
           (->
             (advent19.vm:start-vm-with #(1 1 1 987654321 4 987654321 99)
                                        :write-fn (lambda (vmi val) (push val res)))
             (advent19.vm:wait-vm))
           (first res)))))

(test test-day09-part1-real
  (bind ((in (advent19.d09:input))
         (res (advent19.d09:part-1 in)))
    (is (= 1 (length res)))
    (is (= 3507134798 (first res)))))

