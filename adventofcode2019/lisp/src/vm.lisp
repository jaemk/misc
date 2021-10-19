(defpackage advent19.vm
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :make-vm
    :read-code-from-file
    :run-vm
    :start-vm
    :run-vm-with
    :start-vm-with
    :wait-vm
    :reset-vm
    :parse-op-code

    ;; accessors
    :vm-code
    :vm-name
    :vm-in-ch
    :vm-stdout
    ))
(in-package advent19.vm)
(named-readtables:in-readtable :interpol-syntax)

(defclass vm ()
  ((code
     :initarg :code
     :initform (vector)
     :accessor vm-code)
   (og-code
     :initform (vector)
     :accessor vm-og-code)
   (name
     :initarg :name
     :initform (format nil "~a" (uuid:make-v4-uuid))
     :reader vm-name)
   (in-ch
     :initform (make-instance 'chanl:bounded-channel :size 10)
     :reader vm-in-ch)
   (stdout
     :initarg :stdout
     :initform *standard-output*
     :accessor vm-stdout)
   (write-fn
     :initarg :write-fn
     :initform (lambda (vmi val) (format (vm-stdout vmi) "~a~&" val))
     :accessor vm-write-fn)
   (handle
     :initform nil
     :accessor vm-thread-handle)))


(defun read-code-from-file (file-path)
  (->>
    (str:from-file file-path)
    (str:trim)
    (str:split #?",")
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)
    ((lambda (l) (coerce l 'vector)))))

(defun make-vm (code &key (noun nil) (verb nil) (stdout nil) (write-fn nil))
  (bind ((code (coerce (copy-seq code) 'vector))
         (og-code (copy-seq code))
         (vmi (make-instance 'vm :code code)))
    (setf (vm-og-code vmi) og-code)
    (when noun
      (setf (aref (vm-code vmi) 1) noun))
    (when verb
      (setf (aref (vm-code vmi) 2) verb))
    (when stdout
      (setf (vm-stdout vmi) stdout))
    (when write-fn
      (setf (vm-write-fn vmi) write-fn))
    vmi))

(defun start-vm-with (&rest args)
  (log:debug "starting vm with ~a~%" args)
  (bind ((vmi (apply #'make-vm args)))
    (start-vm vmi)))

(defun run-vm-with (&rest args)
  (->
    (apply #'start-vm-with args)
    (wait-vm)))

(defmethod start-vm ((vmi vm))
  (bind ((handle (bt:make-thread (lambda () (do-run-vm vmi)))))
    (setf (vm-thread-handle vmi) handle)
    vmi))

(defmethod wait-vm ((vmi vm))
  (->
    (vm-thread-handle vmi)
    bt:join-thread)
  vmi)

(defmethod reset-vm ((vmi vm))
  (->>
    (vm-og-code vmi)
    (setf (vm-code vmi)))
  vmi)

; (defun to-mode (c)
;   (cond ((eql #\1 c) :imd)
;         ((eql nil c) :pos)
;         ((eql #\0 c) :pos)
;         (t (error #?"found ${c} in mode place, expected 1's"))))

; (defun parse-op-code (oc)
;   (if (> 100 oc)
;     (values oc :pos :pos :pos)
;     (bind ((ocs #?"${oc}")
;            (len (length ocs))
;            (op (parse-integer (str:substring (- len 2) len ocs)))
;            (chars (map 'list #'identity (str:substring 0 (- len 2) ocs)))
;            (modes (reverse chars)))
;       (values
;         op
;         (to-mode (nth 0 modes))
;         (to-mode (nth 1 modes))
;         (to-mode (nth 2 modes))
;         ))))

;;; not sure if the math way is safe since this assumes only
;;; 1's will be in higher places, so keeping above around for now
(defun parse-op-code (oc)
  (if (> 100 oc)
    (values oc :pos :pos :pos)
    (bind ((remainder oc)
           (m1 :pos)
           (m2 :pos)
           (m3 :pos))
      (when (<= 10000 remainder)
        (setf m3 :imd)
        (decf remainder 10000))
      (when (<= 1000 remainder)
        (setf m2 :imd)
        (decf remainder 1000))
      (when (<= 100 remainder)
        (setf m1 :imd)
        (decf remainder 100))
      (values remainder m1 m2 m3))))

(defun do-run-vm (vmi)
  (bind ((code (vm-code vmi))
         (in-ch (vm-in-ch vmi))
         (write-fn (vm-write-fn vmi))
         (ptr 0))
    (log:trace "vm ~a running code:~%~a" (vm-name vmi) code)
    (loop
      do
        (progn
          (bind ((op-code (aref code ptr))
                 ((:values op m1 m2 m3) (parse-op-code op-code)))
            (log:trace "code: ~a >> op: ~a, modes: (~a, ~a, ~a)" op-code op m1 m2 m3)
            (case op
              (99 (return))
              ;; add
              (1 (progn
                   (do-add ptr code m1 m2)
                   (setf ptr (+ 4 ptr))))
              ;; mul
              (2 (progn
                   (do-mul ptr code m1 m2)
                   (setf ptr (+ 4 ptr))))
              ;; read
              (3 (progn
                   (do-read ptr code in-ch)
                   (setf ptr (+ 2 ptr))))
              ;; write
              (4 (progn
                   (do-write ptr code m1 write-fn vmi)
                   (setf ptr (+ 2 ptr))))
              ;; jump if true
              (5 (bind ((new-ptr (do-jump-if-true ptr code m1 m2)))
                   (if new-ptr
                     (setf ptr new-ptr)
                     (setf ptr (+ 3 ptr)))))
              ;; jump if false
              (6 (bind ((new-ptr (do-jump-if-false ptr code m1 m2)))
                   (if new-ptr
                     (setf ptr new-ptr)
                     (setf ptr (+ 3 ptr)))))
              ;; less than
              (7 (progn
                   (do-less-than ptr code m1 m2)
                   (setf ptr (+ 4 ptr))))
              ;; equals
              (8 (progn
                   (do-equals ptr code m1 m2)
                   (setf ptr (+ 4 ptr))))
              ))))
    (log:debug "vm ~a complete" (vm-name vmi))))

(defun val-in-mode (code ptr mode)
  (if (eql :imd mode)
    ptr
    (aref code ptr)))

(defun do-jump-if-true (ptr code m1 m2)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-1-val (val-in-mode code in-1-ptr m1))
         (in-2-val (val-in-mode code in-2-ptr m2)))
    (when (not (zerop in-1-val))
      in-2-val)))

(defun do-jump-if-false (ptr code m1 m2)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-1-val (val-in-mode code in-1-ptr m1))
         (in-2-val (val-in-mode code in-2-ptr m2)))
    (when (zerop in-1-val)
      in-2-val)))

(defun do-less-than (ptr code m1 m2)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (val-in-mode code in-1-ptr m1))
         (in-2-val (val-in-mode code in-2-ptr m2)))
    (if (< in-1-val in-2-val)
      (setf (aref code in-3-ptr) 1)
      (setf (aref code in-3-ptr) 0))))

(defun do-equals (ptr code m1 m2)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (val-in-mode code in-1-ptr m1))
         (in-2-val (val-in-mode code in-2-ptr m2)))
    (if (= in-1-val in-2-val)
      (setf (aref code in-3-ptr) 1)
      (setf (aref code in-3-ptr) 0))))

(defun do-add (ptr code m1 m2)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (val-in-mode code in-1-ptr m1))
         (in-2-val (val-in-mode code in-2-ptr m2)))
    (setf (aref code in-3-ptr) (+ in-1-val in-2-val))))

(defun do-mul (ptr code m1 m2)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (val-in-mode code in-1-ptr m1))
         (in-2-val (val-in-mode code in-2-ptr m2)))
    (setf (aref code in-3-ptr) (* in-1-val in-2-val))))

(defun do-read (ptr code in-ch)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (value (chanl:recv in-ch :blockp t)))
    (setf (aref code in-1-ptr) value)))

(defun do-write (ptr code m1 write-fn vmi)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (value (val-in-mode code in-1-ptr m1)))
    (funcall write-fn vmi value)))

