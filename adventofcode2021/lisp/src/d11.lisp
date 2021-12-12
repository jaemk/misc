(defpackage advent.d11
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run)
  (:import-from :advent.utils
                :make-hashset
                :hashset-insert
                :hashset-get
                :hashset-length
                :hashset-pop))
(in-package advent.d11)
(named-readtables:in-readtable :interpol-syntax)

(defclass grid ()
  ((inner
     :initarg :inner
     :initform (error "grid :inner required")
     :accessor grid-inner)))

(defun make-grid (height width &key (initial-element nil))
  (bind ((ie (or initial-element 0))
         (inner (make-array (list height width) :initial-element ie))
         (g (make-instance 'grid :inner inner)))
    g))

(defmethod grid-dims ((g grid))
  (array-dimensions (grid-inner g)))

(defmethod grid-set ((g grid) x y val)
  (bind ((inner (grid-inner g)))
    (setf (aref inner y x) val))
  g)

(defmethod grid-inc ((g grid) x y &key (by nil))
  (bind ((by (or by 1))
         (inner (grid-inner g)))
    (incf (aref inner y x) by)))

(defmethod grid-get ((g grid) x y)
  (aref (grid-inner g) y x))

(defmethod grid-around ((g grid) x y)
  (bind ((res nil)
         ((h w) (grid-dims g)))
    (when (< 0 x)
      ; add left
      (push (list (1- x) y) res)
      ; add top-left
      (when (< 0 y)
        (push (list (1- x) (1- y)) res))
      ; add bottom-left
      (when (> (1- h) y)
        (push (list (1- x) (1+ y)) res)))
    (when (< x (1- w))
      ; add right
      (push (list (1+ x) y) res)
      ; add top-right
      (when (< 0 y)
        (push (list (1+ x) (1- y)) res))
      ; add bottom-right
      (when (> (1- h) y)
        (push (list (1+ x) (1+ y)) res)))
    (when (< 0 y)
      (push (list x (1- y)) res))
    (when (< y (1- w))
      (push (list x (1+ y)) res))
    res))

(defmethod grid-print ((g grid))
  (bind (((h w) (grid-dims g)))
    (loop for y from 0 below h do
          (progn
            (format t "~&")
            (loop for x from 0 below w do
                  (format t "~a" (aref g y x))))))
  (format t "~%"))

(defmethod grid-copy ((g grid))
  (bind (((h w) (grid-dims g))
         (res (make-grid h w)))
    (loop for y from 0 below h do
          (loop for x from 0 below w do
                (grid-set res x y (grid-get g x y))))
    res))

(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?|\n|)
    (mapcar #'str:trim)
    (remove-if #'str:empty?)
    (lambda (lines)
      (bind ((height (length lines))
             (width (length (first lines)))
             (g (make-grid height width)))
        (loop for line in lines
              for y from 0 do
              (loop for c across line
                    for x from 0 do
                    (grid-set g x y (- (char-code c) #.(char-code #\0)))))
        g))))

(defun input ()
  (->>
    (str:from-file "../input/d11.txt")
    (parse)))

(defun part-1 (input &key (steps nil) (stop nil))
  (bind ((input (grid-copy input))
         (flashes 0)
         (max-steps (or steps 100))
         (stop (or stop (lambda (num-steps _) (>= num-steps max-steps))))
         (steps 0)
         ((height width) (grid-dims input)))
    (loop while (not (funcall stop steps input)) do
          (bind ((flashed (make-hashset)))
            (incf steps)
            ;; bump by 1
            (loop for y from 0 below height do
                  (loop for x from 0 below width do
                        (grid-inc input x y)))
            ;; explode 9s
            (bind ((flashers (make-hashset)))
              ;; find initial flashers
              (loop for y from 0 below height do
                    (loop for x from 0 below width do
                          (when (< 9 (grid-get input x y))
                            (hashset-insert flashers (list x y))
                            (hashset-insert flashed (list x y))
                            )))
              ;; flash and track new flashers
              (loop while (< 0 (hashset-length flashers)) do
                    (bind (((x y) (hashset-pop flashers))
                           (others (grid-around input x y)))
                      (incf flashes)
                      (grid-set input x y 0)
                      (loop for (ox oy) in others do
                            (progn
                              (when (not (hashset-get flashed (list ox oy)))
                                (grid-inc input ox oy)
                                (when (and (< 9 (grid-get input ox oy)))
                                  (hashset-insert flashed (list ox oy))
                                  (hashset-insert flashers (list ox oy)))))))))))
    (values flashes steps)))

(defun part-2 (input)
  (flet ((stop (_ g)
          (block b
            (bind (((h w) (grid-dims g)))
              (loop for y from 0 below h do
                    (loop for x from 0 below w do
                          (when (not (zerop (grid-get g x y)))
                            (return-from b nil)))))
            t)))
    (bind ((input (grid-copy input))
           ((:values _ steps) (part-1 input :stop #'stop)))
      steps)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

