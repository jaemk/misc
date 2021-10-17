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

