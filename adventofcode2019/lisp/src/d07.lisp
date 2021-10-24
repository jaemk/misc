(defpackage advent19.d07
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :make-chain
    :reset-chain
    :start-chain
    :send-chain
    :wait-chain
    :part-1
    :part-2
    :run))
(in-package advent19.d07)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (advent19.vm:read-code-from-file "../input/d07.txt"))

(defun make-chain (code size &key (last-write nil))
  "return a chain of VMs from open input to output
   IN -> 0 -> 1 -> 2 -> 3 -> 4 -> OUT"
  (bind ((chain nil)
         (next nil))
    (loop for i from 1 to size
          do (bind ((write-fn (if next
                                (bind ((to-vm (advent19.vm:get-vm-name next))
                                       (to-ch (advent19.vm:get-vm-in-ch next)))
                                  (lambda (vminst val)
                                    (log:trace "sending ~a from ~a to ~a"
                                               val
                                               (advent19.vm:get-vm-name vminst)
                                               to-vm)
                                    (chanl:send to-ch val)))
                                last-write))
                    (vmi (advent19.vm:make-vm code :name #?"vm-${(- size i)}" :write-fn write-fn)))
               (push vmi chain)
               (setf next vmi)))
    chain))

(defun reset-chain (vm-chain)
  (dolist (vmi vm-chain)
    (advent19.vm:reset-vm vmi)))

(defun start-chain (vm-chain)
  (dolist (vmi vm-chain)
    (advent19.vm:start-vm vmi)))

(defun fill-chain-args (args size)
  (bind ((n (- size (length args)))
         (pad (loop repeat n collect nil))
         (args (copy-list args)))
    (nconc args pad)
    args))

(defun send-chain (vm-chain args)
  (bind ((args (fill-chain-args args (length vm-chain)))
         (arg-to-vm (mapcar #'list args vm-chain)))
    (log:trace "~a" arg-to-vm)
    (loop for (arg vmi) in (reverse arg-to-vm)
          when arg
          do (advent19.vm:send-vm vmi arg))))

(defun wait-chain (vm-chain)
  (dolist (vmi (reverse vm-chain))
    (advent19.vm:wait-vm vmi)))

(defun part-1 (in)
  (bind ((res nil)
         (m nil)
         (p nil)
         (chain (make-chain
                  in
                  5
                  :last-write (lambda (vmi val) (setf res val)))))
    (cl-permutation:doperms (perm 5) ;; a permutation of (1 2 3 4 5)
      (bind ((perm (cl-permutation:perm-to-list perm))
             ;; lower it to (0 1 2 3 4)
             (perm (mapcar (lambda (n) (- n 1)) perm)))
        (log:trace "running perm: ~a" perm)
        (reset-chain chain)
        (start-chain chain)
        (send-chain chain perm)
        (send-chain chain '(0))
        (wait-chain chain)
        (log:trace "perm: ~a, result: ~a" perm res)
        (when (or (null m) (> res m))
          (setf m res)
          (setf p perm))))
    (values m p)))

(defun part-2 (in)
  (bind ((m nil)
         (p nil)
         (chain (make-chain
                  in
                  5)))
    (setf (advent19.vm:vm-write-fn (first (last chain))) (lambda (vmi val)
                                                           (advent19.vm:send-vm
                                                             (first chain)
                                                             val)))
    (cl-permutation:doperms (perm 5) ;; a permutation of (1 2 3 4 5)
      (bind ((perm (cl-permutation:perm-to-list perm))
             ;; increase it to (5 6 7 8 9)
             (perm (mapcar (lambda (n) (+ n 4)) perm)))
        (log:trace "running perm: ~a" perm)
        (reset-chain chain)
        (send-chain chain perm)
        (send-chain chain '(0))
        (start-chain chain)
        (wait-chain chain)
        (bind ((head (first chain))
               (res (chanl:recv (advent19.vm:get-vm-in-ch head) :blockp t)))
          (log:trace "perm: ~a, result: ~a" perm res)
          (when (or (null m) (> res m))
            (setf m res)
            (setf p perm)))))
    (values m p)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

