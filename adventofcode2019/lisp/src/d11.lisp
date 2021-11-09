(defpackage advent19.d11
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :run))
(in-package advent19.d11)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (advent19.vm:read-code-from-file "../input/d11.txt"))

(defclass robot ()
  ((vm
     :initform nil
     :accessor robot-vm)
   (dir
     :initform 'up
     :accessor robot-dir)
   (pos
     :initform (list 0 0)
     :accessor robot-pos)
   (grid
     :initform (make-hash-table :test #'equal)
     :accessor robot-grid)
   (out
     :initform nil
     :accessor robot-out)))


(defun make-robot (code)
  (bind (
         (r (make-instance 'robot))
         (out (make-instance 'chanl:bounded-channel :size 100))
         (write-fn (lambda (vmi val)
                     (log:trace "writing ~a" val)
                     (chanl:send out val)))
         (vmi (advent19.vm:make-vm code :write-fn write-fn))
         )
    (setf (robot-vm r) vmi)
    (setf (robot-out r) out)
    r))

(defun turn (current dir)
  (cond
    ((eq current 'up) (if (eq dir 'right) 'right 'left))
    ((eq current 'right) (if (eq dir 'right) 'down 'up))
    ((eq current 'down) (if (eq dir 'right) 'left 'right))
    ((eq current 'left) (if (eq dir 'right) 'up 'down))
    (t (error #?|unknown direction ${current}|))
    ))

(defmethod robot-step ((r robot))
  (bind ((dir (robot-dir r))
         (pos (robot-pos r)))
    (cond
      ((eq dir 'up) (incf (second pos)))
      ((eq dir 'right) (incf (first pos)))
      ((eq dir 'down) (decf (second pos)))
      ((eq dir 'left) (decf (first pos)))
      ))
  r)


(defmethod robot-run ((r robot) &key (initial 0))
  (bind ((buf nil)
         (minx nil)
         (miny nil)
         (maxx nil)
         (maxy nil)
         (first-loop t))
    (advent19.vm:start-vm (robot-vm r))
    (loop
      do (bind ((vmi (robot-vm r))
                ((x y) (robot-pos r))
                (pos #?|${x},${y}|)
                (grid (robot-grid r))
                (pos-val (if first-loop
                           initial
                           (progn
                             (setf first-loop nil)
                             (gethash pos grid))))
                )
           (setf minx (if (null minx) x (min minx x)))
           (setf miny (if (null miny) y (min miny y)))
           (setf maxx (if (null maxx) x (max maxx x)))
           (setf maxy (if (null maxy) y (max maxy y)))
           (alexandria:if-let (out (chanl:recv (robot-out r) :blockp nil))
             (push out buf)
             nil)
           (cond
             ((= (length buf) 2)
               (bind ((dir (pop buf))
                      (color (pop buf)))
                 (log:trace "count:~a, dir:~a, pos: ~a, buf: ~a"
                            (hash-table-count grid) (robot-dir r) (robot-pos r) buf)
                 (log:trace "color:~a, dir:~a" color dir)
                 (setf (gethash pos grid) color)
                 (if (= 1 dir)
                   (setf (robot-dir r) (turn (robot-dir r) 'right))
                   (setf (robot-dir r) (turn (robot-dir r) 'left)))
                 (robot-step r)))
             ((endp buf)
               (when (advent19.vm:get-vm-is-done vmi)
                 (return r))
               (when (advent19.vm:get-vm-is-reading vmi)
                 (advent19.vm:send-vm
                   vmi
                   (if (or (null pos-val) (zerop pos-val))
                      0
                      1))))
             (t nil))
          )
      )
    (list minx miny maxx maxy)))

(defmethod robot-count-painted ((r robot))
  (hash-table-count (robot-grid r)))

(defun part-1 (in)
  (bind ((r (make-robot in)))
    (robot-run r)
    (robot-count-painted r)))

(defun part-2 (in)
  (bind ((r (make-robot in))
         ((minx miny maxx maxy) (robot-run r :initial 1))
         (out (make-string-output-stream))
         )
    ; (format out "~&printing from ~a,~a to ~a,~a~%" minx miny maxx maxy)
    (format out "~%")
    (loop for yy from miny to maxy
          for yyy = (- maxy yy)
          for y = (+ miny yyy)
          do (progn
               (loop for x from minx to maxx
                     do (bind ((xy #?|${x},${y}|)
                               (val (gethash xy (robot-grid r)))
                               (s (if (eq 1 val) "X" " ")))
                          (format out "~a" s)))
               (format out "~%")))
    (get-output-stream-string out)))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

