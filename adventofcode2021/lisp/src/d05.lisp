(defpackage advent.d05
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run))
(in-package advent.d05)
(named-readtables:in-readtable :interpol-syntax)

(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?|\n|)
    (mapcar (lambda (line)
              (->>
                (str:split " -> " line)
                (mapcar (lambda (pair)
                          (mapcar #'parse-integer (str:split "," pair)))))))))

(defun input ()
  (->>
    (str:from-file "../input/d05.txt")
    (parse)))

(defun part-1 (input)
  (bind ((grid (make-hash-table :test #'equal))
         (two-plus 0))
    (loop for start-end in input do
          (bind ((((x1 y1) (x2 y2)) start-end))
            (cond
              ((= x1 x2)
                (loop for y from (min y1 y2) to (max y1 y2) do
                      (bind ((key #?|${x1},${y}|)
                             (existing (or (gethash key grid) 0)))
                        (setf (gethash key grid) (1+ existing)))))
              ((= y1 y2)
                (loop for x from (min x1 x2) to (max x1 x2) do
                      (bind ((key #?|${x},${y1}|)
                             (existing (or (gethash key grid) 0)))
                        (setf (gethash key grid) (1+ existing)))))
              (t nil))))
    (maphash (lambda (k v) (when (>= v 2) (incf two-plus))) grid)
    two-plus))

(defun part-2 (input)
  (bind ((grid (make-hash-table :test #'equal))
         (two-plus 0))
    (loop for start-end in input do
          (bind ((((x1 y1) (x2 y2)) start-end)
                 (x-dir (cond ((> x1 x2) #'1-) ((= x1 x2) #'identity) (t #'1+)))
                 (y-dir (cond ((> y1 y2) #'1-) ((= y1 y2) #'identity) (t #'1+)))
                 (x x1)
                 (y y1))
            (loop do
                  (bind ((key #?|${x},${y}|)
                         (existing (or (gethash key grid) 0)))
                    (setf (gethash key grid) (1+ existing))
                    (setf x (funcall x-dir x))
                    (setf y (funcall y-dir y)))
                  until (and (= x (funcall x-dir x2)) (= y (funcall y-dir y2))))))
    (maphash (lambda (k v)
               (when (>= v 2)
                 (incf two-plus)))
             grid)
    two-plus))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

