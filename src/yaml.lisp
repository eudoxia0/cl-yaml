(in-package :cl-user)
(defpackage yaml
  (:use :cl)
  (:export :parse)
  (:documentation "The main YAML interface."))
(in-package :yaml)

(defgeneric parse (input)
  (:documentation "Parse a YAML string or a pathname to a YAML file into Lisp
 data."))

(defmethod parse ((input string))
  (yaml.parser:parse-string input))

(defmethod parse ((input pathname))
  (parse (uiop:read-file-string input)))
