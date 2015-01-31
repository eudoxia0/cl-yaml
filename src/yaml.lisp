(in-package :cl-user)
(defpackage yaml
  (:use :cl)
  (:export :parse
           :emit
           :emit-to-string)
  (:documentation "The main YAML interface."))
(in-package :yaml)

(defgeneric parse (input)
  (:documentation "Parse a YAML string or a pathname to a YAML file into Lisp
 data."))

(defmethod parse ((input string))
  (yaml.parser:parse-string input))

(defmethod parse ((input pathname))
  (parse (uiop:read-file-string input)))

(defun emit (value stream)
  (yaml.emitter:emit value stream))

(defun emit-to-string (value)
  (yaml.emitter:emit-to-string value))
