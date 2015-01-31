(in-package :cl-user)
(defpackage yaml.emitter
  (:use :cl)
  (:export :encode
           :emit
           :emit-to-string)
  (:documentation "The YAML emitter."))
(in-package :yaml.emitter)

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

(defun emit (value stream)
  (encode value stream))

(defun emit-to-string (value)
  (with-output-to-string (stream)
    (emit value stream)))
