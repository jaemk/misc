(defpackage advent.d07
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :series-sum
    :fuel
    :run))
(in-package advent.d07)
(named-readtables:in-readtable :interpol-syntax)


(defun input ()
  (->>
    (str:from-file "../input/d07.txt")
    (str:split ",")
    (mapcar #'str:trim)
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)))

(defun part-1 (input)
  (bind ((len (length input))
         (pos (coerce input 'vector))
         (minimum most-positive-fixnum)
         (minpos nil))
    ;; this is technically wrong since we're only looking
    ;; at positions that crabs are currently on, but part-1
    ;; works anyway, probably just to mess with you when it
    ;; comes to part-2. Leaving it as is since it works :)
    (dotimes (i len)
      (bind ((num 0))
        (dotimes (j len)
          (incf num (abs (- (aref pos i) (aref pos j)))))
        (when (< num minimum)
          (setf minimum num)
          (setf minpos (aref pos i)))))
    (list minpos minimum)))

(defun series-sum (start step count)
  ; = (n/2) (2a + (n − 1)d)
  (->>
    (1- count)
    (* step)
    (+ (* 2 start))
    (* (/ count 2))))

(defun fuel (a b)
  (bind ((diff (abs (- a b))))
    (series-sum 1 1 diff)))

(defun part-2 (input)
  (bind ((len (length input))
         (start (apply #'min input))
         (end (apply #'max input))
         (pos (coerce input 'vector))
         (minimum most-positive-fixnum)
         (minpos nil))
    (loop for i from start to end do
      (bind ((num 0))
        (dotimes (j len)
          (incf num (fuel i (aref pos j))))
        (when (< num minimum)
          (setf minimum num)
          (setf minpos i))))
    (list minpos minimum)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

