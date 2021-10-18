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

    ;; accessors
    :vm-code
    :vm-name
    :vm-in-ch
    ))
(in-package advent19.vm)
(named-readtables:in-readtable :interpol-syntax)

(defclass vm ()
  ((code
     :initarg :code
     :initform (vector)
     :accessor vm-code)
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
  (bind ((vmi (make-instance 'vm :code (coerce (copy-seq code) 'vector))))
    (when noun
      (setf (aref (vm-code vmi) 1) noun))
    (when verb
      (setf (aref (vm-code vmi) 2) verb))
    (when stdout
      (setf (vm-stdout vmi) stdout))
    (when write-fn
      (setf (vm-write-fn vmi) write-fn))
    vmi))

(defun start-vm-with (&rest args)  ;(code &key (noun nil) (verb nil))
  (log:debug "starting vm with ~a~%" args)
  (bind ((vmi (apply #'make-vm args)))
    (start-vm vmi)))

(defun run-vm-with (&rest args)  ;(code &key (noun nil) (verb nil))
  (->
    (apply #'start-vm-with args)
    (wait-vm)))

;(bind ((res nil)
;       (vmi (run-vm-with #(3 0 4 0 99) :write-fn (lambda (vmi val) (push val res))))
;       (in-ch (vm-in-ch vmi)))
;  (chanl:send in-ch 1)
;  (wait-vm vmi)
;  (log:debug "wrote back: ~a" res))

(defmethod start-vm ((vmi vm))
  (bind ((handle (bt:make-thread (lambda () (do-run-vm vmi)))))
    (setf (vm-thread-handle vmi) handle)
    vmi))

(defmethod wait-vm ((vmi vm))
  (-> (vm-thread-handle vmi) bt:join-thread) vmi)

(defun do-run-vm (vmi)
  (bind ((code (vm-code vmi))
         (in-ch (vm-in-ch vmi))
         (write-fn (vm-write-fn vmi))
         (ptr 0))
    (loop
      do
        (progn
          (bind ((v (aref code ptr)))
            (case v
              (99 (return))
              (1 (progn
                   (do-add ptr code)
                   (setf ptr (+ 4 ptr))))
              (2 (progn
                   (do-mul ptr code)
                   (setf ptr (+ 4 ptr))))
              (3 (progn
                   (do-read ptr code in-ch)
                   (setf ptr (+ 2 ptr))))
              (4 (progn
                   (do-write ptr code write-fn vmi)
                   (setf ptr (+ 2 ptr))))
              ))))
    (log:debug "vm ~a complete" (vm-name vmi))))

(defun do-add (ptr code)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (aref code in-1-ptr))
         (in-2-val (aref code in-2-ptr)))
    (setf (aref code in-3-ptr) (+ in-1-val in-2-val))))

(defun do-mul (ptr code)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (in-2-ptr (aref code (+ 2 ptr)))
         (in-3-ptr (aref code (+ 3 ptr)))
         (in-1-val (aref code in-1-ptr))
         (in-2-val (aref code in-2-ptr)))
    (setf (aref code in-3-ptr) (* in-1-val in-2-val))))

(defun do-read (ptr code in-ch)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (value (chanl:recv in-ch :blockp t)))
    (setf (aref code in-1-ptr) value)))

(defun do-write (ptr code write-fn vmi)
  (bind ((in-1-ptr (aref code (+ 1 ptr)))
         (value (aref code in-1-ptr)))
    (funcall write-fn vmi value)))

