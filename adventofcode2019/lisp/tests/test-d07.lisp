(in-package :advent19/tests)
(named-readtables:in-readtable :interpol-syntax)

(def-suite* test-d07 :in all)

(test test-day07-basic-chain
  (bind ((res nil)
         (chain (advent19.d07:make-chain
                  #(3 0 4 0 99)
                  10
                  :last-write (lambda (vmi val) (push val res)))))
    (advent19.d07:reset-chain chain)
    (advent19.d07:start-chain chain)
    (advent19.d07:send-chain chain '(33))
    (advent19.d07:wait-chain chain)
    (is (= 33 (first res)))))

(test test-day07-basic-chain-reused
  (bind ((res nil)
         (chain (advent19.d07:make-chain
                  #(3 0 4 0 99)
                  10
                  :last-write (lambda (vmi val) (push val res)))))
    (advent19.d07:reset-chain chain)
    (advent19.d07:start-chain chain)
    (advent19.d07:send-chain chain '(33))
    (advent19.d07:wait-chain chain)
    (is (= 33 (first res)))

    ;; again
    (advent19.d07:reset-chain chain)
    (advent19.d07:start-chain chain)
    (advent19.d07:send-chain chain '(44))
    (advent19.d07:wait-chain chain)
    (is (= 44 (first res)))))

(test test-day07-phase-chain-01
  (bind ((res nil)
         (chain (advent19.d07:make-chain
                  #(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0)
                  5
                  :last-write (lambda (vmi val) (push val res)))))
    (advent19.d07:reset-chain chain)
    (advent19.d07:start-chain chain)
    (advent19.d07:send-chain chain '(4 3 2 1 0))
    (advent19.d07:send-chain chain '(0))
    (advent19.d07:wait-chain chain)
    (is (= 43210 (first res)))))

(test test-day07-phase-chain-02
  (bind ((res nil)
         (chain (advent19.d07:make-chain
                  #(3 23 3 24 1002 24 10 24 1002 23 -1 23
                    101 5 23 23 1 24 23 23 4 23 99 0 0)
                  5
                  :last-write (lambda (vmi val) (push val res)))))
    (advent19.d07:reset-chain chain)
    (advent19.d07:start-chain chain)
    (advent19.d07:send-chain chain '(0 1 2 3 4))
    (advent19.d07:send-chain chain '(0))
    (advent19.d07:wait-chain chain)
    (is (= 54321 (first res)))))

(test test-day07-phase-chain-03
  (bind ((res nil)
         (chain (advent19.d07:make-chain
                  #(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
                    1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0)
                  5
                  :last-write (lambda (vmi val) (push val res)))))
    (advent19.d07:reset-chain chain)
    (advent19.d07:start-chain chain)
    (advent19.d07:send-chain chain '(1 0 4 3 2))
    (advent19.d07:send-chain chain '(0))
    (advent19.d07:wait-chain chain)
    (is (= 65210 (first res)))))

(test test-day07-part1-01
  (bind (((:values res perm) (advent19.d07:part-1
                               #(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))))
    (is (= 43210 res))
    (is (equal (list 4 3 2 1 0) perm))))

(test test-day07-part1-02
  (bind (((:values res perm) (advent19.d07:part-1
                               #(3 23 3 24 1002 24 10 24 1002 23 -1 23
                                 101 5 23 23 1 24 23 23 4 23 99 0 0))))
    (is (= 54321 res))
    (is (equal (list 0 1 2 3 4) perm))))

(test test-day07-part1-03
  (bind (((:values res perm) (advent19.d07:part-1
                               #(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
                                 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0))))
    (is (= 65210 res))
    (is (equal (list 1 0 4 3 2) perm))))

(test test-day07-part1-real
  (bind ((res (advent19.d07:part-1 (advent19.d07:input))))
    (is (= 914828 res))))

(test test-day07-part2-01
  (bind (((:values res perm) (advent19.d07:part-2
                               #(3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
                                 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5))))
    (is (= 139629729 res))
    (is (equal (list 9 8 7 6 5) perm))))

(test test-day07-part2-02
  (bind (((:values res perm) (advent19.d07:part-2
                               #(3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54
                                 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4
                                 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10))))
    (is (= 18216 res))
    (is (equal (list 9 7 8 5 6) perm))))

(test test-day07-part2-real
  (bind ((res (advent19.d07:part-2 (advent19.d07:input))))
    (is (= 17956613 res))))

