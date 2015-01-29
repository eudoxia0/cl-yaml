(in-package :cl-user)
(defpackage yaml.error
  (:use :cl)
  (:export :yaml-error
           :parsing-error
           :message
           :line
           :column)
  (:documentation "YAML errors."))
(in-package :yaml.error)

(define-condition yaml-error ()
  ()
  (:documentation "The base class of all YAML conditions."))

(define-condition parsing-error (yaml-error)
  ((message :reader message
            :initarg :message
            :type string
            :documentation "The error message.")
   (line :reader line
         :initarg :line
         :type integer
         :documentation "The line where the error happened.")
   (column :reader column
           :initarg :column
           :type integer
           :documentation "The column where the error happened."))
  (:report
   (lambda (condition stream)
     (format stream "Parsing error at line ~A, column ~A: ~A.~&"
             (line condition)
             (column condition)
             (message condition))))
  (:documentation "An error when parsing a YAML file."))
