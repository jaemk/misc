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
    ;; accessors
    :vm-code
    :vm-mem
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
    :mem-get
    ))
(in-package advent19.vm)
(named-readtables:in-readtable :interpol-syntax)

;; TODO:
;;   - make vm-code a method that concats all memory pages
;;   - remove vm-code accessor
;;   - use mem-get for all access instead of vm-code

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
         (pages (make-hash-table :test #'eq)))
    (loop for code-page in paged-code
          for i from 0
          do (bind ((start (* i page-size))
                    (page (make-page start :code code-page)))
               (setf (gethash start pages) page)))
    (make-instance 'memory :pages pages :page-cache (gethash 0 pages))))

(defmethod print-object ((m memory) stream)
  (print-unreadable-object (m stream :type t)
    (with-accessors ((pages memory-pages)) m
      (bind ((starts (alexandria:hash-table-keys pages))
             (starts (sort starts #'<)))
        (format stream "~{~a~^, ~}" starts)))))

(defmethod mem-page-start-for-index ((m memory) index)
  (bind (((:values page-start-num rel-index) (floor index page-size)))
    (values (* page-start-num page-size) rel-index)))

(defun get-or-create-page (page-start-num pages)
  (alexandria:if-let (page (gethash page-start-num pages))
    page
    (bind ((new-page (make-page page-start-num)))
      (setf (gethash page-start-num pages) new-page)
      new-page)))

(defmethod mem-goc-page-for-index ((m memory) index)
  (bind (((:values page-start-num rel-index) (mem-page-start-for-index m index))
         (page-cache (memory-page-cache m))
         (page-cache-start (page-start page-cache))
         (pages (memory-pages m)))
    (if (= page-cache-start page-start-num)
      (values page-cache rel-index)
      (values (get-or-create-page page-start-num pages) rel-index))))

(defmethod mem-get ((m memory) index)
  (bind (((:values page rel-index) (mem-goc-page-for-index m index)))
    (aref (page-code page) rel-index)))


(defclass vm ()
  ((mem
     :initarg :mem
     :initform (error "vm :mem required")
     :accessor vm-mem)
   (code
     :initarg :code
     :initform (error "vm :code required")
     :accessor vm-code)
   (og-code
     :initform nil
     :accessor vm-og-code)
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
         (og-code (copy-seq code))
         (mem (make-memory code))
         (vmi (make-instance 'vm :mem mem :code code)))
    (setf (vm-og-code vmi) og-code)
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
    (vm-og-code vmi)
    #'copy-seq
    (setf (vm-code vmi)))
  (setf (vm-thread-handle vmi) nil)
  (log:trace "reset vm ~a" (vm-name vmi))
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
  (bind ((name (vm-name vmi))
         (code (vm-code vmi))
         (in-ch (vm-in-ch vmi))
         (write-fn (vm-write-fn vmi))
         (ptr 0))
    (log:trace "vm ~a running code:~%~a" name code)
    (loop
      do
        (progn
          (bind ((op-code (aref code ptr))
                 ((:values op m1 m2 m3) (parse-op-code op-code)))
            (log:trace "~a code: ~a >> op: ~a, modes: (~a, ~a, ~a)" name op-code op m1 m2 m3)
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
    (log:trace "vm ~a complete" (vm-name vmi))))

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

