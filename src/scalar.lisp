(in-package :cl-user)
(defpackage yaml.parser
  (:use :cl)
  (:export :parse-scalar)
  (:documentation "Parser for scalar values."))
(in-package :yaml.parser)

;;; Constants

(defparameter +null+ nil
  "The NULL constant. Nil by default.")

(defparameter +false+ nil
  "The falsehood constant. Nil by default.")

;;; Regular expressions or lists of names

(defparameter +null-names+
  (list "null" "Null" "NULL" "~"))

(defparameter +true-names+
  (list "true" "True" "TRUE"))

(defparameter +false-names+
  (list "false" "False" "FALSE"))

(defparameter +integer-scanner+
  (ppcre:create-scanner "[-+]?[0-9]+"))

(defparameter +octal-integer-scanner+
  (ppcre:create-scanner "0o[0-7]+"))

(defparameter +hex-integer-scanner+
  (ppcre:create-scanner "0x[0-9a-fA-F]+"))

(defparameter +float-scanner+
  (ppcre:create-scanner
   "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?"))

(defparameter +infinity-scanner+
  (ppcre:create-scanner "[-+]?(\\.inf|\\.Inf|\\.INF)"))

(defparameter +nan-names+
  (list ".nan" ".NaN" ".NAN"))

;;; The actual parser

(defun parse-scalar (string)
  "Parse a YAML scalar string into a Lisp scalar value."
  (cond
    ;; Null
    ((member string +null-names+ :test #'equal)
     +null+)
    ;; Truth and falsehood
    ((member string +true-names+ :test #'equal)
     t)
    ((member string +false-names+ :test #'equal)
     +false+)
    ;; Integers
    ;; Floating-point numbers
    ((member string +nan-names+ :test #'equal)
     +not-a-number+)
    (t
     string)))
