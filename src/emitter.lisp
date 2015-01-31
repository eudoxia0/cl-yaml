(in-package :cl-user)
(defpackage yaml.emitter
  (:use :cl)
  (:export :encode
           :emit
           :emit-to-string)
  (:documentation "The YAML emitter."))
(in-package :yaml.emitter)

;;; Encoder functions

(defgeneric encode (value stream)
  (:documentation "Write the YAML corresponding to value to a stream."))

(defmethod encode ((true (eql 't)) stream)
  "Encode true."
  (write-string "true" stream))

(defmethod encode ((true (eql 'nil)) stream)
  "Encode false."
  (write-string "false" stream))

(defmethod encode ((integer integer) stream)
  "Encode an integer."
  (princ integer stream))

(defmethod encode ((float float) stream)
  "Encode a float."
  (princ float stream))

(defmethod encode ((list list) stream)
  (write-string "[" stream)
  (loop for sublist on list do
    (encode (first sublist) stream)
    (when (rest sublist)
      (write-string ", " stream)))
  (write-string "]" stream))

(defmethod encode ((vector vector) stream)
  (encode (loop for elem across vector collecting elem) stream))

;;; Interface

(defun emit (value stream)
  (encode value stream))

(defun emit-to-string (value)
  (with-output-to-string (stream)
    (emit value stream)))
