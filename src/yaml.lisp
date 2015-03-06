(in-package :cl-user)
(defpackage yaml
  (:use :cl)
  (:export :parse
           :emit
           :emit-to-string)
  (:documentation "The main YAML interface."))
(in-package :yaml)

(defgeneric parse (input &key single-document-p)
  (:documentation "Parse a YAML string or a pathname to a YAML file into Lisp
 data."))

(defmethod parse ((input string) &key single-document-p)
  (let ((parsed (yaml.parser:parse-string input)))
    (if single-document-p
        (second parsed)
        parsed)))

(defmethod parse ((input pathname) &key single-document-p)
  (parse (uiop:read-file-string input)
         :single-document-p single-document-p))

(defun emit (value stream)
  (yaml.emitter:emit value stream))

(defun emit-to-string (value)
  (yaml.emitter:emit-to-string value))
