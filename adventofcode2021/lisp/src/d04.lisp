(defpackage advent.d04
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run))
(in-package advent.d04)
(named-readtables:in-readtable :interpol-syntax)

(defclass grid ()
  ((rows
     :initarg :rows
     :initform (error "grid :rows required")
     :accessor grid-rows)
   (wins
     :initarg :wins
     :initform (error "grid :wins required")
     :accessor grid-wins)
   (vals
     :initarg :vals
     :initform (error "grid :vals required")
     :accessor grid-vals)
   (hits
     :initform nil
     :accessor grid-hits)))

(defmethod grid-add ((g grid) move)
  (when (advent.utils:hashset-get (grid-vals g) move)
    (push move (grid-hits g))
    (loop for win in (grid-wins g) do
          (progn
            (advent.utils:hashset-remove win move)
            (when (advent.utils:hashset-empty? win)
              (return g))))))

(defun board->grid (board)
  (->>
    (str:trim board)
    (str:split #?|\n|)
    (map 'vector (lambda (line)
                   (map 'vector #'parse-integer
                        (remove-if #'str:empty? (str:split " " line)))))
    (lambda (rows)
      (bind ((vals (advent.utils:make-hashset))
             (wins nil))
        (loop for r across rows do
              (bind ((win (advent.utils:make-hashset)))
                (advent.utils:hashset-extend win (coerce r 'list))
                (push win wins)))
        (loop for i from 0 to (1- (length (aref rows 0))) do
              (bind ((win (advent.utils:make-hashset)))
                (loop for row across rows do
                      (progn
                        (advent.utils:hashset-insert win (aref row i))
                        (advent.utils:hashset-insert vals (aref row i))))
                (push win wins)))
        (make-instance 'grid
                       :rows rows
                       :vals vals
                       :wins wins)))))

(defun parse (s)
  (bind ((groups (str:split #?|\n\n| (str:trim s)))
         (moves (first groups))
         (moves (mapcar #'parse-integer (str:split "," moves)))
         (boards (rest groups))
         (grids (mapcar #'board->grid boards)))
    (list moves grids)))

(defun input ()
  (->>
    (str:from-file "../input/d04.txt")
    (parse)))

(defun part-1 (input)
  (bind (((moves grids) input)
         (drawn (advent.utils:make-hashset)))
    (loop do
      (bind ((next (pop moves)))
        (when (null next)
          (error "ran out of moves..."))
        (advent.utils:hashset-insert drawn next)
        (loop for grid in grids do
              (alexandria:when-let (winning-grid (grid-add grid next))
                (return-from part-1
                  (* next
                    (loop for row across (grid-rows grid)
                          sum (loop for n across row
                                    sum (if (advent.utils:hashset-get drawn n) 0 n)))))))))))

(defun part-2 (input)
  nil)


(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

