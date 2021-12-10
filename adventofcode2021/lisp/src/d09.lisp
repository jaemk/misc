(defpackage advent.d09
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run)
  (:import-from :advent.utils
                :make-hashset
                :hashset-get
                :hashset-insert
                :hashset-length))
(in-package advent.d09)
(named-readtables:in-readtable :interpol-syntax)

(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?|\n|)
    (mapcar #'str:trim)
    (remove-if #'str:empty?)
    (lambda (lines)
      (bind ((height (length lines))
             (width (length (first lines)))
             (a (make-array (list height width) :initial-element 0)))
        (loop for line in lines
              for y from 0 do
              (loop for c across line
                   for x from 0 do
                   (setf (aref a y x) (- (char-int c) #.(char-int #\0)))))
        a))))

(defun input ()
  (->>
    (str:from-file "../input/d09.txt")
    (parse)))

(defun around (height width x y)
  (bind ((res nil))
    (when (< 0 x)
      (push (list (1- x) y) res))
    (when (< x (1- width))
      (push (list (1+ x) y) res))
    (when (< 0 y)
      (push (list x (1- y)) res))
    (when (< y (1- height))
      (push (list x (1+ y)) res))
    res))

(defun find-low-points (input)
  (bind ((height (array-dimension input 0))
         (width (array-dimension input 1))
         (mins nil)
         (min-coords nil))
    (loop for y from 0 to (1- height) do
          (loop for x from 0 to (1- width) do
                (bind ((coords (around height width x y))
                       (this (aref input y x))
                       (others (loop for (ox oy) in coords collect
                                     (aref input oy ox))))
                  (when (and (not (apply #'= (cons this others)))
                             (< this (first (sort others #'<))))
                    (push this mins)
                    (push (list x y) min-coords)))))
    (values mins min-coords)))

(defun part-1 (input)
  (apply #'+ (mapcar #'1+ (find-low-points input))))

(defun map-basin (input x y)
  (bind ((height (array-dimension input 0))
         (width (array-dimension input 1))
         (next (list (list x y)))
         (basin (make-hashset :from (list (list x y)))))
    (loop while next do
          (bind (((x y) (pop next))
                 (more (around height width x y)))
            (loop for (mx my) in more do
                  (bind ((val (aref input my mx))
                         (point (list mx my)))
                    (when (and (not (= 9 val))
                               (not (hashset-get basin point)))
                      (hashset-insert basin point)
                      (push point next))))))
    basin))

(defun part-2 (input)
  (bind (((:values _ coords) (find-low-points input))
         (basins (make-hash-table :test #'equal)))
    (loop for (x y) in coords do
          (bind ((basin (map-basin input x y)))
            (setf (gethash (list x y) basins) basin)))
    (bind ((sizes nil)
           (_ (maphash (lambda (_ v) (push (hashset-length v) sizes)) basins))
           (biggest (subseq (sort sizes #'>) 0 3)))
      (apply #'* biggest))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

