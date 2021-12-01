(defpackage advent/tests
  (:use :cl
        :metabang-bind
        :arrow-macros
        :fiveam
        :advent
        )
  (:export :all))
(in-package :advent/tests)

(def-suite all
  :description "Tests")

(setf fiveam:*on-failure* :backtrace)
(setf fiveam:*on-error* :backtrace)

(log:config (advent.config:value :log-level))
(log:config :sane2)
(log:config :nofile)

