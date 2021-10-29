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
    (t (error #?"unknown direction ${current}"))
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


(defmethod robot-run ((r robot))
  (bind ((buf nil))
    (advent19.vm:start-vm (robot-vm r))
    (loop
      do (bind ((vmi (robot-vm r))
                ((x y) (robot-pos r))
                (pos #?"${x},${y}")
                (grid (robot-grid r))
                (pos-val (gethash pos grid))
                )
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
      )))

(defmethod robot-count-painted ((r robot))
  (hash-table-count (robot-grid r)))

(defun part-1 (in)
  (bind ((r (make-robot in)))
    (robot-run r)
    (robot-count-painted r)))


(defun part-2 (in)
  nil)

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

