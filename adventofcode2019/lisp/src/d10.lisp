(defpackage advent19.d10
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :grid->coords
    :delta->polar-angle
    :part-1
    :part-2
    :run))
(in-package advent19.d10)
(named-readtables:in-readtable :interpol-syntax)


(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?"\n")
    (map 'vector (lambda (line)
                   (map 'vector (lambda (c) (if (eql c #\#) 1 0)) line)))))

(defun input ()
  (->
    (str:from-file "../input/d10.txt")
    (parse)))


(defun grid->coords (grid)
  (loop for row across grid
        for y from 0
        append (loop for col across row
                     for x from 0
                     when (= 1 col)
                       collect (list x y))))

(defun radians->degrees (rads)
  (* rads (/ 180 pi)))

(defun cartesian->polar-degrees (x y)
  "return a radial degree from 0->90.
   +x +y => +
   -x -y => +
   any neg => -"
  (->
    (atan (/ y x))
    (radians->degrees)
    (* 100)
    (round)
    (/ 100)
    (float)
    (abs)))

(defun pos (n) (> n 0))
(defun neg (n) (< n 0))

(defun delta->polar-angle (dx dy)
  (cond
    ((= 0 dy) (if (> dx 0) 0 180)) ; 0 or pi
    ((= 0 dx) (if (> dy 0) 90 270)) ; pi/2 or 3pi/2
    ((and (pos dx) (pos dy)) (+ 0 (cartesian->polar-degrees dx dy))) ; 0 -> pi/2
    ((and (neg dx) (pos dy)) (+ 90 (cartesian->polar-degrees dx dy))) ; pi/2 -> pi
    ((and (neg dx) (neg dy)) (+ 180 (cartesian->polar-degrees dx dy))) ; pi -> 3pi/2
    ((and (pos dx) (neg dy)) (+ 270 (cartesian->polar-degrees dx dy))) ; 3pi/2 -> 2pi
    (t (error (format nil "unhandled dx:~a dy:~a" dx dy)))))

(defun part-1 (in)
  (bind ((coords (grid->coords in)))
    (->
      (loop for coord in coords
            collect (bind ((angles (make-hash-table :test #'eql)))
                      (loop for other in coords
                           when (not (equal coord other))
                             do (bind (((x y) coord)
                                       ((ox oy) other)
                                       (dx (- ox x))
                                       (dy (- oy y))
                                       (angle (delta->polar-angle dx dy)))
                                  (setf (gethash angle angles) t)))
                      (list (hash-table-count angles) coord)))
      (sort (lambda (a b) (> (first a) (first b))))
      (first))))

(defun part-2 (in)
  nil)

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

