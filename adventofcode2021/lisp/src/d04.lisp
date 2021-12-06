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
  ((num
     :initarg :num
     :initform 0
     :accessor grid-num)
   (rows
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
   (won?
     :initform nil
     :accessor grid-won?)))

(defmethod grid-add ((g grid) move &key (continue nil))
  (when (grid-won? g)
    (return-from grid-add nil))
  (when (advent.utils:hashset-get (grid-vals g) move)
    (bind ((won nil))
      (loop for win in (grid-wins g) do
            (progn
              (advent.utils:hashset-remove win move)
              (when (advent.utils:hashset-empty? win)
                (setf won g)
                (when (null continue)
                  (return)))))
      (when won
        (setf (grid-won? g) t))
      won)))

(defun board->grid (i board)
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
                (advent.utils:hashset-insert-all win (coerce r 'list))
                (push win wins)))
        (loop for i from 0 to (1- (length (aref rows 0))) do
              (bind ((win (advent.utils:make-hashset)))
                (loop for row across rows do
                      (progn
                        (advent.utils:hashset-insert win (aref row i))
                        (advent.utils:hashset-insert vals (aref row i))))
                (push win wins)))
        (make-instance 'grid
                       :num i
                       :rows rows
                       :vals vals
                       :wins wins)))))

(defun parse (s)
  (bind ((groups (str:split #?|\n\n| (str:trim s)))
         (moves (first groups))
         (moves (mapcar #'parse-integer (str:split "," moves)))
         (boards (rest groups))
         (grids (loop for b in boards for i from 1 collect (board->grid i b))))
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
  (bind (((moves grids) input)
         (drawn (advent.utils:make-hashset))
         (wins 0)
         (last-winning-move nil)
         (last-winner nil))
    (loop do
      (bind ((next (pop moves)))
        (when (or (null next) (= wins (length grids)))
          (return))
        (advent.utils:hashset-insert drawn next)
        (loop for grid in grids do
              (alexandria:when-let (winning-grid (grid-add grid next :continue t))
                (incf wins)
                (setf last-winning-move next)
                (setf last-winner winning-grid)))))
    (* last-winning-move
       (bind ((uncalled (advent.utils:make-hashset))
              (total 0))
         (loop for win in (grid-wins last-winner) do
               (advent.utils:hashset-map win
                                         (lambda (v) (advent.utils:hashset-insert uncalled v))))
         (advent.utils:hashset-map uncalled (lambda (v) (incf total v)))
         total))))


(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

