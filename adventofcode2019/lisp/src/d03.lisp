(defpackage advent19.d03
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run))
(in-package advent19.d03)
(named-readtables:in-readtable :interpol-syntax)

(defun parse (s)
  (->> s
    (str:trim)
    (str:split #?"\n")
    (mapcar #'str:trim)
    (mapcar (lambda (s) (str:split #?"," s)))))

(defun input ()
  (->>
    (str:from-file "../input/d03.txt")
    (parse)))

(defun watch-steps (moves seen &key (collect-intersections nil))
  "Iterate over all moves, tracking seen locations and steps to reach.
   If collecting, return a list of intersections and the total number of steps
   taken to reach the intersection by each line.
   ( (steps (x y)), ... )"
  (bind ((loc (list 0 0))
         (steps 0)
         (ints nil))
    (loop for m in moves do
      (progn
        (bind ((dir (str:s-first m))
               (amount (parse-integer (str:s-rest m))))
          (loop for _ from 1 to amount do
            (progn
              (incf steps)
              (cond
                ((string= dir "U") (incf (second loc)))
                ((string= dir "D") (decf (second loc)))
                ((string= dir "L") (decf (first loc)))
                ((string= dir "R") (incf (first loc))))
              (let ((loc- (copy-list loc)))
                (if collect-intersections
                  (let ((prev-steps (gethash loc- seen)))
                    (when prev-steps
                      (push (list (+ steps prev-steps) loc-) ints)))
                  (setf (gethash loc- seen) steps))))))))
    ints))

(defun from-zero (steps-point)
  "For a (steps (x y)) pair, return a new triple of
   (grid-distance steps (x y))"
  (bind (((steps point) steps-point))
    (list (apply #'+ (mapcar #'abs point)) steps point)))

(defun part-1 (in)
  (bind (((a b) in)
         (seen (make-hash-table :test #'equal))
         (intsa (watch-steps a seen))
         (intsb (watch-steps b seen :collect-intersections t))
         (ints (union intsa intsb))
         (dist-steps-ints (mapcar #'from-zero ints)))
    (first (first (sort dist-steps-ints #'< :key #'first)))))

(defun part-2 (in)
  (bind (((a b) in)
         (seen (make-hash-table :test #'equal))
         (intsa (watch-steps a seen))
         (intsb (watch-steps b seen :collect-intersections t))
         (ints (union intsa intsb))
         (dist-steps-ints (mapcar #'from-zero ints)))
    (second (first (sort dist-steps-ints #'< :key #'second)))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

