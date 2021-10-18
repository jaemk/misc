(defpackage advent19/tests
  (:use :cl
        :metabang-bind
        :arrow-macros
        :fiveam
        :advent19)
  (:export :all))
(in-package :advent19/tests)

(def-suite all
  :description "Tests")

(setf fiveam:*on-failure* :backtrace)
(setf fiveam:*on-error* :backtrace)

(log:config (advent19.config:value :log-level))
(log:config :sane2)
(log:config :nofile)

