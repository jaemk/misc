(defpackage advent.d06
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run))
(in-package advent.d06)
(named-readtables:in-readtable :interpol-syntax)


(defun input ()
  (->>
    (str:from-file "../input/d06.txt")
    (str:split ",")
    (mapcar #'str:trim)
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)))

(defun part-1 (input)
  (bind ((fish (make-array
                 (length input)
                 :adjustable t
                 :fill-pointer (length input)
                 :initial-contents input)))
    (dotimes (_ 80)
      (bind ((new 0))
        (loop for f across fish
              for i from 0 do
              (if (zerop f)
                (progn
                  (incf new)
                  (setf (aref fish i) 6))
                (decf (aref fish i))))
        (dotimes (_ new)
          (vector-push-extend 8 fish))))
    (length fish)))

(defun part-2 (input &key (days nil))
  (bind (;; on a 7 day cycle
         (week (make-array '(7 2) :initial-element 0))
         ;; where today is the first day
         (day 0)
         (total-days 0)
         (max-days (or days 256)))
    ;; apply initial state:
    ;; - a timer of "0" means it will spawn on the first day
    ;; - a timer of "1" means it will spawn on the second day
    ;; - a timer of "2" means it will spawn on the third day
    (loop for timer in input do
          (incf (aref week timer 0)))
    (dotimes (_ max-days)
          (bind ((spawn-today (aref week day 0))
                 (spawn-next-week (aref week day 1))
                 (new-spawn-day (mod (+ 2 day) 7)))
            (incf (aref week new-spawn-day 1) spawn-today)
            (incf (aref week day 0) spawn-next-week)
            (setf (aref week day 1) 0)
            (setf day (mod (1+ day) 7))
            (incf total-days)))
    (bind ((sum 0))
      (dotimes (i 7)
        (dotimes (j 2)
          (incf sum (aref week i j))))
      sum)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

