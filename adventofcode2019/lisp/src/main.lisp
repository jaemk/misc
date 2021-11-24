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
    advent19.d05:run
    advent19.d06:run
    advent19.d07:run
    advent19.d08:run
    advent19.d09:run
    advent19.d10:run
    advent19.d11:run
    advent19.d12:run
    advent19.d13:run
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
      (format *error-output* "~&Unhandled error: ~a~%" (advent19.utils:get-error-backtrace c))
      (sb-ext:quit :unix-status 1)))

  (handler-bind
    (
      ;; C-c
      (sb-sys:interactive-interrupt
        (lambda (c)
          (format t "~&Aborting...~%")
          (sb-ext:quit :unix-status 1)))

      ;; everything else
      (error
        (lambda (c)
          (format *error-output* "~&Error: ~a~%" (advent19.utils:get-error-backtrace c))
          (sb-ext:quit :unix-status 1)))
    )
      (progn
        (log:config (advent19.config:value :log-level))
        (log:config :sane2)
        (log:config :nofile)
        (log:debug "args: ~a" argvs)
        (run))))

