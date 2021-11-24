(defpackage advent19.d13
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :get-dims
    :part-2
    :run))
(in-package advent19.d13)
(named-readtables:in-readtable :interpol-syntax)

(defun input ()
  (advent19.vm:read-code-from-file "../input/d13.txt"))

(defun count-blocks (screen)
  (bind ((count 0))
    (maphash
      (lambda (coord tile)
        (when (= 2 tile)
          (incf count)))
      screen)
    count))

(defun startup-die (in)
  (bind ((out (make-instance 'chanl:bounded-channel :size 100))
         (write-fn (lambda (vmi val)
                     (log:trace "writing out ~a" val)
                     (chanl:send out val)))
         (vmi (advent19.vm:make-vm in :write-fn write-fn))
         (screen (make-hash-table :test #'equal))
         (buf nil)
         )
    (advent19.vm:start-vm vmi)
    (loop
      do (cond
           ((advent19.vm:get-vm-is-done vmi)
             (return screen))
           ((= 3 (length buf))
             (bind ((tile (pop buf))
                    (y (pop buf))
                    (x (pop buf))
                    ;(coord #?|${x},${y}|))
                    (coord (list x y)))
               (setf (gethash coord screen) tile)))
           (t
            (alexandria:if-let (out (chanl:recv out :blockp nil))
              (push out buf)
              nil))))))

(defun part-1 (in)
  (->
    (startup-die in)
    (count-blocks)
    ))

(defun get-dims (in)
  (bind ((screen (startup-die in))
         (maxx 0)
         (maxy 0))
    (maphash
      (lambda (coord tile)
        (bind (((x y) coord))
          (when (> x maxx)
            (setf maxx x))
          (when (> y maxy)
            (setf maxy y))))
      screen)
    (list maxx maxy)))

(defun part-2 (in)
  (bind ((dims (get-dims in)))
    dims))

(defun run ()
  (let ((in (input)))
    (bind (((:values res ms) (advent19.utils:with-timing (part-1 in))))
        (format t "~&Part 1 (~ams): ~a" ms res))
    (bind (((:values res ms) (advent19.utils:with-timing (part-2 in))))
        (format t "~&Part 2 (~ams): ~a" ms res))))

