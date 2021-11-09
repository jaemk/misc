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
  "parse line representation in a 2d array
   where `1` denotes an asteroid"
  (->>
    (str:trim s)
    (str:split #?|\n|)
    (map 'vector (lambda (line)
                   (map 'vector (lambda (c) (if (eql c #\#) 1 0)) line)))))

(defun input ()
  (->
    (str:from-file "../input/d10.txt")
    (parse)))

(defun grid->coords (grid)
  "convert the 2d array of asteroids to a list
   of only asteroid coordinates"
  (loop for row across grid
        for y from 0
        append (loop for col across row
                     for x from 0
                     when (= 1 col)
                       collect (list x y))))

(defun radians->degrees (rads)
  "covert radians to a degree from 0->90"
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

(defun pos? (n) (> n 0))
(defun neg? (n) (< n 0))

(defun delta->polar-angle (dx dy)
  "given an x/y delta from some reference point,
   return the polar angle from the reference to the point.
   This will be an angle from 0->360 but it's rotation angle
   (counter/clockwise) depends on the orientation of the x/y
   coordinates being used"
  (cond
    ((= 0 dy) (if (> dx 0) 0 180)) ; 0 or pi
    ((= 0 dx) (if (> dy 0) 90 270)) ; pi/2 or 3pi/2
    ((and (pos? dx) (pos? dy)) (+ 0 (cartesian->polar-degrees dx dy))) ; 0 -> pi/2
    ((and (neg? dx) (pos? dy)) (+ 90 (cartesian->polar-degrees dx dy))) ; pi/2 -> pi
    ((and (neg? dx) (neg? dy)) (+ 180 (cartesian->polar-degrees dx dy))) ; pi -> 3pi/2
    ((and (pos? dx) (neg? dy)) (+ 270 (cartesian->polar-degrees dx dy))) ; 3pi/2 -> 2pi
    (t (error (format nil "unhandled dx:~a dy:~a" dx dy)))))

(defun delta->distance (dx dy)
  "calculate the distance for an x/y delta
   from some reference point"
  (sqrt (+ (expt dx 2) (expt dy 2))))

(defun part-1 (in)
  "find the asteroid that can see some other asteroid
   from the most number of angles"
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

;; ==== part 2 ====

(defun collect-by-angle (coords pos)
  "Collect every angle (and a list of asteroids) at which an
   asteroid can be seen from the reference point `pos`.
   The resulting list should be starting from 'up' and should
   rotate counter-clockwise (rotating to the right in the graph below).
   Since everything's mirrored because y coords are mirrored, we want
   the resulting 'clockwise' list of angles to be starting from
   angle 270->360 (increasing), plus 0->270 (increasing)
   which would equal a normal x/y grid's 90->0 (decreasing), plus 360->90 (decreasing)

   e.g. our polar grid looks like
                270
                 |
      (-11,-2)   |
                 |   (11, -2)
                 |
    180--------(0,0)-----------0
                 |
                 |   (11, 2)
      (-11,2)    |
                 |
                 |
                90
  "
  (bind ((angles (make-hash-table :test #'eql))
         ((x y) pos)
         (with-points-sorted-closest nil)
         (counter-clockwise-from-0 nil)
         (clockwise-from-270 nil)
         (tail nil))
    ;; save every point and its distance from `pos` hashed by angle
    (loop for coord in coords
          do (bind (((ox oy) coord)
                    (dx (- ox x))
                    (dy (- oy y))
                    (angle (delta->polar-angle dx dy))
                    (dist (delta->distance dx dy)))
               (push
                 ;; so each angle has a list whose elements are
                 ;; the following: (distance, angle, point)
                 (list dist angle (list ox oy))
                 (gethash angle angles))))
    ;; transform the hash-by-angles to a list of angle-points
    ;; e.g. ( (2deg ((1-dist 2deg (0 1)) (2-dist 2deg (1 2)))) )
    (maphash (lambda (angle points)
               (push
                 (list angle
                       (sort points (lambda (a b) (< (first a) (first b)))))
                 with-points-sorted-closest))
             angles)
    ;; sort the list from largest to smallest angle, which
    ;; gives us every angle rotating counter-clockwise, starting at 0/360/the right
    ;; and rotating up-left
    (setf counter-clockwise-from-0
          (sort with-points-sorted-closest (lambda (a b) (> (first a) (first b)))))
    ;; go through the counter-clockwise angles to reverse them into a
    ;; clockwise rotation, but one starting at angle 270 instead of 0
    (loop for angle-points in counter-clockwise-from-0
          do (bind ((angle (first angle-points)))
               (if (>= angle 270)
                 (push angle-points clockwise-from-270)
                 (push angle-points tail))))
    (setf (rest (last clockwise-from-270)) tail)
    clockwise-from-270))

(defun part-2 (in pos)
  (bind ((coords (grid->coords in))
         (by-angle (collect-by-angle coords pos))
         (vaporized-count 0)
         (vaporized nil))
    ;; make angle list circular so we can loop forever
    (setf (rest (last by-angle)) by-angle)

    (loop for angle-points in by-angle
          do (bind (((angle points) angle-points)
                    (point (pop points)))
               (when point
                 (bind (((dist angle xy) point))
                   (setf vaporized xy)
                   (incf vaporized-count))))
          when (= 200 vaporized-count)
            do (bind (((x y) vaporized))
                 (return (+ (* x 100) y))))))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in)))
           ((visible pos) res))
        (format t "~&Part 1 (~ams): ~a" ms res)
      (bind (((:values res ms) (advent19.utils:with-timing (part-2 in pos))))
          (format t "~&Part 2 (~ams): ~a" ms res)))))

