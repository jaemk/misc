(defpackage advent19.vm
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    ;; vm
    :make-vm
    :read-code-from-file
    :run-vm
    :start-vm
    :send-vm
    :run-vm-with
    :start-vm-with
    :wait-vm
    :reset-vm
    :parse-op-code
    :get-vm-code
    ;; accessors
    :vm-name
    :vm-in-ch
    :vm-stdout
    :vm-write-fn

    ;; page
    :make-page

    ;; mem
    :make-memory
    :partition-code
    ;; accessors
    ))
(in-package advent19.vm)
(named-readtables:in-readtable :interpol-syntax)


(defparameter page-size 64)

(defclass page ()
  ((start
     :initarg :start
     :initform (error "page :start is required")
     :accessor page-start
     :documentation "the starting index of the page in virtual memory space")
   (code
     :initarg :code
     :initform (make-array (list page-size) :initial-element 0)
     :accessor page-code)))

(defun make-page (start &key (code nil) (item-absloc nil))
  (bind ((args (list 'page :start start))
         (args (if code (append args (list :code code)) args))
         (p (apply #'make-instance args)))
    (when item-absloc
      (bind (((item absloc) item-absloc)
             (loc (- absloc start)))
        (setf (aref (page-code p) loc) item)))
    p))

(defmethod print-object ((p page) stream)
  (print-unreadable-object (p stream :type t)
    (with-accessors ((start page-start)) p
      (format stream "~a" start))))


(defclass memory ()
  ((pages
     :initarg :pages
     :initform (error "memory :pages is required")
     :accessor memory-pages)
   (max-page-start
     :initarg :max-page-start
     :initform (error "memory :max-page-start is required")
     :accessor memory-max-page-start)
   (page-cache
     :initarg :page-cache
     :initform (error "memory :page-cache is required")
     :accessor memory-page-cache)))

(defun partition-code (code page-size)
  (bind ((code-len (length code)))
    (loop for start from 0 to code-len by page-size
          for remaining = (- code-len start)
          for offset = (min remaining page-size)
          collect (subseq code start (+ start offset)))))

(defun make-memory (code)
  (bind ((paged-code (partition-code code page-size))
         (pages (make-hash-table :test #'eq))
         (max-page-start 0))
    (loop for code-page in paged-code
          for i from 0
          do (bind ((start (* i page-size))
                    (page (make-page start :code code-page)))
               (setf max-page-start start)
               (setf (gethash start pages) page)))
    (make-instance 'memory
                   :pages pages
                   :max-page-start max-page-start
                   :page-cache (gethash 0 pages))))

(defmethod print-object ((m memory) stream)
  (print-unreadable-object (m stream :type t)
    (with-accessors ((pages memory-pages)) m
      (bind ((starts (alexandria:hash-table-keys pages))
             (starts (sort starts #'<)))
        (format stream "~{~a~^, ~}" starts)))))

(defmethod mem-page-start-for-index ((m memory) index)
  (bind (((:values page-start-num rel-index) (floor index page-size)))
    (values (* page-start-num page-size) rel-index)))

(defmethod get-or-create-page ((m memory) page-start-num pages)
  (alexandria:if-let (page (gethash page-start-num pages))
    page
    (bind ((new-page (make-page page-start-num)))
      (setf (gethash page-start-num pages) new-page)
      (setf (memory-max-page-start m) (max (memory-max-page-start m) page-start-num))
      new-page)))

(defmethod mem-goc-page-for-index ((m memory) index)
  (bind (((:values page-start-num rel-index) (mem-page-start-for-index m index))
         (page-cache (memory-page-cache m))
         (page-cache-start (page-start page-cache))
         (pages (memory-pages m)))
    (if (= page-cache-start page-start-num)
      (values page-cache rel-index)
      (values (get-or-create-page m page-start-num pages) rel-index))))

(defmethod mem-set ((m memory) index val)
  (bind (((:values page rel-index) (mem-goc-page-for-index m index)))
    (->
      (aref (page-code page) rel-index)
      (setf val))))

(defmethod mem-get ((m memory) index)
  (bind (((:values page rel-index) (mem-goc-page-for-index m index)))
    (aref (page-code page) rel-index)))

(defmethod mem-range ((m memory) start end)
  (loop for i from start to (1- end)
        collect (mem-get m i)))


(defclass vm ()
  ((mem
     :initarg :mem
     :initform (error "vm :mem required")
     :accessor vm-mem)
   (code
     :initarg :code
     :initform (error "vm :code required")
     :accessor vm-code)
   (name
     :initarg :name
     :initform (format nil "~a" (uuid:make-v4-uuid))
     :accessor vm-name)
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

(defmethod print-object ((vmi vm) stream)
  (print-unreadable-object (vmi stream :type t)
    (with-accessors ((mem vm-mem)) vmi
      (format stream "~a" (mem-get mem 0)))))


(defun read-code-from-file (file-path)
  (->>
    (str:from-file file-path)
    (str:trim)
    (str:split #?",")
    (remove-if #'str:empty?)
    (mapcar #'parse-integer)
    ((lambda (l) (coerce l 'vector)))))


(defun prepare-code (code noun verb)
  (when noun
    (setf (aref code 1) noun))
  (when verb
    (setf (aref code 2) verb))
  code)

(defun make-vm (code &key (name nil) (noun nil) (verb nil) (stdout nil) (write-fn nil))
  (bind ((code (coerce (copy-seq code) 'vector))
         (code (prepare-code code noun verb))
         (mem (make-memory code))
         (vmi (make-instance 'vm :mem mem :code code)))
    (when name
      (setf (vm-name vmi) name))
    (when stdout
      (setf (vm-stdout vmi) stdout))
    (when write-fn
      (setf (vm-write-fn vmi) write-fn))
    vmi))

(defun start-vm-with (&rest args)
  (bind ((vmi (apply #'make-vm args)))
    (start-vm vmi)))

(defun run-vm-with (&rest args)
  (->
    (apply #'start-vm-with args)
    (wait-vm)))

(defmethod start-vm ((vmi vm))
  (bind ((handle (bt:make-thread (lambda () (do-run-vm vmi)))))
    (setf (vm-thread-handle vmi) handle)
  (log:trace "started vm ~a" (vm-name vmi))
    vmi))

(defmethod send-vm ((vmi vm) val)
  (-> (vm-in-ch vmi) (chanl:send val))
  (log:trace "sent ~a to ~a" val (vm-name vmi))
  vmi)

(defmethod wait-vm ((vmi vm))
  (->
    (vm-thread-handle vmi)
    bt:join-thread)
  (log:trace "waited for vm ~a" (vm-name vmi))
  vmi)

(defmethod reset-vm ((vmi vm))
  (->>
    (vm-code vmi)
    #'copy-seq
    (make-memory)
    (setf (vm-mem vmi)))
  (setf (vm-thread-handle vmi) nil)
  (log:trace "reset vm ~a" (vm-name vmi))
  vmi)

(defmethod get-vm-code ((vmi vm) start end)
  (->
    (vm-mem vmi)
    (mem-range start end)))

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
      (when (<= 20000 remainder)
        (setf m3 :rel)
        (decf remainder 20000))
      (when (<= 10000 remainder)
        (setf m3 :imd)
        (decf remainder 10000))
      (when (<= 2000 remainder)
        (setf m2 :rel)
        (decf remainder 2000))
      (when (<= 1000 remainder)
        (setf m2 :imd)
        (decf remainder 1000))
      (when (<= 200 remainder)
        (setf m1 :rel)
        (decf remainder 200))
      (when (<= 100 remainder)
        (setf m1 :imd)
        (decf remainder 100))
      (values remainder m1 m2 m3))))

(defun do-run-vm (vmi)
  (bind ((name (vm-name vmi))
         (code (vm-code vmi))
         (mem (vm-mem vmi))
         (in-ch (vm-in-ch vmi))
         (write-fn (vm-write-fn vmi))
         (ptr 0))
    (log:trace "vm ~a running code:~%~a" name code)
    (loop
      do
        (progn
          (bind ((op-code (mem-get mem ptr))
                 ((:values op m1 m2 m3) (parse-op-code op-code)))
            (log:trace "~a code: ~a >> op: ~a, modes: (~a, ~a, ~a)" name op-code op m1 m2 m3)
            (case op
              (99 (return))
              ;; add
              (1 (progn
                   (do-add ptr mem m1 m2)
                   (setf ptr (+ 4 ptr))))
              ;; mul
              (2 (progn
                   (do-mul ptr mem m1 m2)
                   (setf ptr (+ 4 ptr))))
              ;; read
              (3 (progn
                   (do-read ptr mem in-ch)
                   (setf ptr (+ 2 ptr))))
              ;; write
              (4 (progn
                   (do-write ptr mem m1 write-fn vmi)
                   (setf ptr (+ 2 ptr))))
              ;; jump if true
              (5 (bind ((new-ptr (do-jump-if-true ptr mem m1 m2)))
                   (if new-ptr
                     (setf ptr new-ptr)
                     (setf ptr (+ 3 ptr)))))
              ;; jump if false
              (6 (bind ((new-ptr (do-jump-if-false ptr mem m1 m2)))
                   (if new-ptr
                     (setf ptr new-ptr)
                     (setf ptr (+ 3 ptr)))))
              ;; less than
              (7 (progn
                   (do-less-than ptr mem m1 m2)
                   (setf ptr (+ 4 ptr))))
              ;; equals
              (8 (progn
                   (do-equals ptr mem m1 m2)
                   (setf ptr (+ 4 ptr))))
              ))))
    (log:trace "vm ~a complete" (vm-name vmi))))

(defun val-in-mode (mem ptr mode)
  (if (eql :imd mode)
    ptr
    (mem-get mem ptr)))

(defun do-jump-if-true (ptr mem m1 m2)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (in-2-ptr (mem-get mem (+ 2 ptr)))
         (in-1-val (val-in-mode mem in-1-ptr m1))
         (in-2-val (val-in-mode mem in-2-ptr m2)))
    (when (not (zerop in-1-val))
      in-2-val)))

(defun do-jump-if-false (ptr mem m1 m2)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (in-2-ptr (mem-get mem (+ 2 ptr)))
         (in-1-val (val-in-mode mem in-1-ptr m1))
         (in-2-val (val-in-mode mem in-2-ptr m2)))
    (when (zerop in-1-val)
      in-2-val)))

(defun do-less-than (ptr mem m1 m2)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (in-2-ptr (mem-get mem (+ 2 ptr)))
         (in-3-ptr (mem-get mem (+ 3 ptr)))
         (in-1-val (val-in-mode mem in-1-ptr m1))
         (in-2-val (val-in-mode mem in-2-ptr m2)))
    (if (< in-1-val in-2-val)
      (mem-set mem in-3-ptr 1)
      (mem-set mem in-3-ptr 0))))

(defun do-equals (ptr mem m1 m2)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (in-2-ptr (mem-get mem (+ 2 ptr)))
         (in-3-ptr (mem-get mem (+ 3 ptr)))
         (in-1-val (val-in-mode mem in-1-ptr m1))
         (in-2-val (val-in-mode mem in-2-ptr m2)))
    (if (= in-1-val in-2-val)
      (mem-set mem in-3-ptr 1)
      (mem-set mem in-3-ptr 0))))

(defun do-add (ptr mem m1 m2)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (in-2-ptr (mem-get mem (+ 2 ptr)))
         (in-3-ptr (mem-get mem (+ 3 ptr)))
         (in-1-val (val-in-mode mem in-1-ptr m1))
         (in-2-val (val-in-mode mem in-2-ptr m2)))
    (mem-set mem in-3-ptr (+ in-1-val in-2-val))))

(defun do-mul (ptr mem m1 m2)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (in-2-ptr (mem-get mem (+ 2 ptr)))
         (in-3-ptr (mem-get mem (+ 3 ptr)))
         (in-1-val (val-in-mode mem in-1-ptr m1))
         (in-2-val (val-in-mode mem in-2-ptr m2)))
    (mem-set mem in-3-ptr (* in-1-val in-2-val))))

(defun do-read (ptr mem in-ch)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (value (chanl:recv in-ch :blockp t)))
    (mem-set mem in-1-ptr value)))

(defun do-write (ptr mem m1 write-fn vmi)
  (bind ((in-1-ptr (mem-get mem (+ 1 ptr)))
         (value (val-in-mode mem in-1-ptr m1)))
    (funcall write-fn vmi value)))

