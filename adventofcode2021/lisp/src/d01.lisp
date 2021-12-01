(defpackage advent.d01
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :run))
(in-package advent.d01)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (->>
    (str:from-file "../input/d01.txt")
    (str:split #?|\n|)
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)))

(defun part-1 (input)
  (bind ((prev nil)
         (total 0))
    (loop for val in input
          when (and (not (null prev))
                    (> val prev))
            do (incf total)
          do (setf prev val))
    total))

(defun part-2 (input)
  (bind ((input (copy-list input))
         (prev-sum nil)
         (total 0)
         (a0 (pop input))
         (b0 (pop input)))
    (loop for c in input
              and b = b0 then c
              and a = a0 then b
              for sum = (+ a b c)
              when (and (not (null prev-sum))
                        (> sum prev-sum))
                do (incf total)
              do (setf prev-sum sum))
    total))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

