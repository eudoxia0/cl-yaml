(in-package :cl-user)
(defpackage yaml.float
  (:use :cl)
  (:export :*float-strategy*)
  (:documentation "Handle IEEE floating point values."))
(in-package :yaml.float)

(defparameter *float-strategy* :keyword)

#+sbcl
(defparameter *sbcl-nan-value*
  (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero)
    (- sb-ext:double-float-positive-infinity
       sb-ext:double-float-positive-infinity)))

(defun nan-value ()
  (case *float-strategy*
    (:error
     (error 'yaml.error:unsupported-float-value))
    (:keyword
     :NaN)
    (:best-effort
     #+allegro #.excl:*nan-double*
     #+sbcl *sbcl-nan-value*
     #-(or allegro sbcl) :NaN)))
