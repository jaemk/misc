(defpackage advent19.utils
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :now-millis
    :get-error-backtrace
    :with-timing
    :aget
    :trim-to-nil))

(defpackage advent19.config
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :value
    :*values*
    :load-values))

(defpackage advent19.d01
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :part-1
    :part-2
    :run))

(defpackage advent19.d02
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :exec
    :part-1
    :part-2
    :run))

(defpackage advent19.d03
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :input
    :parse
    :part-1
    :part-2
    :run))

(defpackage advent19
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :main))

(in-package :advent19)

