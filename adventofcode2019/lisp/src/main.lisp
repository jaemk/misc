(defpackage advent19
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :main))
(in-package :advent19)
(named-readtables:in-readtable :interpol-syntax)


(defparameter *days*
  '(
    advent19.d01:run
    advent19.d02:run
    advent19.d03:run
    advent19.d04:run
    ))


(defun run ()
  (loop for (day run-day) in (mapcar #'list (alexandria:iota (length *days*) :start 1) *days*) do
    (format t "~&=======================~%* Day ~a *~%" day)
    (bind (((:values r ms) (advent19.utils:with-timing (funcall run-day))))
      (format t "~&>>> ~ams~%" ms))
    ))


(defun main (argvs)
  ;; handle any errors if they aren't cause by the catch-all handler in 'main
  (setf
    *debugger-hook*
    (lambda (c old-hook)
      (declare (ignore old-hook))
      (format *error-output* "~&Unhandled error: ~a~%" c)
      (sb-ext:quit :unix-status 1)))

  (handler-case
    (progn
      (log:config (advent19.config:value :log-level))
      (log:config :sane2)
      (log:config :nofile)
      (log:debug "args: ~a" argvs)
      (run))

    ;; C-c
    (sb-sys:interactive-interrupt
      ()
      (progn
        (format t "~&Aborting...~%")
        (sb-ext:quit :unix-status 1)))

    ;; everything else
    (error
      (e)
      (progn
        (format *error-output* "~&Error: ~a~%" e)
        (sb-ext:quit :unix-status 1)))))

