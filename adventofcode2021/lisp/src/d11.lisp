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

(defun parse (s)
  (->>
    (str:trim s)
    (str:split #?|\n|)
    (mapcar #'str:trim)
    (remove-if #'str:empty?)
    (lambda (lines)
      (bind ((height (length lines))
             (width (length (first lines)))
             (grid (make-array (list height width) :initial-element 0)))
        (loop for line in lines
              for y from 0 do
              (loop for c across line
                    for x from 0 do
                    (setf (aref grid y x) (- (char-code c) #.(char-code #\0)))))
        grid))))

(defun input ()
  (->>
    (str:from-file "../input/d11.txt")
    (parse)))

(defun around (height width x y)
  (bind ((res nil))
    (when (< 0 x)
      ; add left
      (push (list (1- x) y) res)
      ; add top-left
      (when (< 0 y)
        (push (list (1- x) (1- y)) res))
      ; add bottom-left
      (when (> (1- width) y)
        (push (list (1- x) (1+ y)) res)))
    (when (< x (1- width))
      ; add right
      (push (list (1+ x) y) res)
      ; add top-right
      (when (< 0 y)
        (push (list (1+ x) (1- y)) res))
      ; add bottom-right
      (when (> (1- width) y)
        (push (list (1+ x) (1+ y)) res)))
    (when (< 0 y)
      (push (list x (1- y)) res))
    (when (< y (1- height))
      (push (list x (1+ y)) res))
    res))

(defun print-grid (g)
  (bind (((h w) (array-dimensions g)))
    (loop for y from 0 below h do
          (progn
            (format t "~&")
            (loop for x from 0 below w do
                  (format t "~a" (aref g y x))))))
  (format t "~%"))

(defun copy-grid (g)
  (bind (((h w) (array-dimensions g))
         (res (make-array (list h w) :initial-element 0)))
    (loop for y from 0 below h do
          (loop for x from 0 below w do
                (setf (aref res y x) (aref g y x))))
    res))

(defun part-1 (input &key (steps nil) (stop nil))
  (bind ((input (copy-grid input))
         (flashes 0)
         (max-steps (or steps 100))
         (stop (or stop (lambda (num-steps _) (>= num-steps max-steps))))
         (steps 0)
         ((height width) (array-dimensions input)))
    (loop while (not (funcall stop steps input)) do
          (bind ((flashed (make-hashset)))
            (incf steps)
            ;; bump by 1
            (loop for y from 0 below height do
                  (loop for x from 0 below width do
                        (incf (aref input y x))))
            ;; explode 9s
            (bind ((flashers (make-hashset)))
              ;; find initial flashers
              (loop for y from 0 below height do
                    (loop for x from 0 below width do
                          (when (< 9 (aref input y x))
                            (hashset-insert flashers (list x y))
                            (hashset-insert flashed (list x y))
                            )))
              ;; flash and track new flashers
              (loop while (< 0 (hashset-length flashers)) do
                    (bind (((x y) (hashset-pop flashers))
                           (others (around height width x y)))
                      (incf flashes)
                      (setf (aref input y x) 0)
                      (loop for (ox oy) in others do
                            (progn
                              (when (not (hashset-get flashed (list ox oy)))
                                (incf (aref input oy ox))
                                (when (and (< 9 (aref input oy ox)))
                                  (hashset-insert flashed (list ox oy))
                                  (hashset-insert flashers (list ox oy)))))))))))
    (values flashes steps)))

(defun part-2 (input)
  (flet ((stop (_ grid)
          (block b
            (bind (((h w) (array-dimensions grid)))
              (loop for y from 0 below h do
                    (loop for x from 0 below w do
                          (when (not (zerop (aref grid y x)))
                            (return-from b nil)))))
            t)))
    (bind ((input (copy-grid input))
           ((:values _ steps) (part-1 input :stop #'stop)))
      steps)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

