(in-package :cl-user)
(defpackage yaml.parser
  (:use :cl)
  (:import-from :alexandria
                :destructuring-case)
  (:import-from :libyaml.macros
                :with-parser
                :with-event)
  (:export :parse)
  (:documentation "The YAML parser."))
(in-package :yaml.parser)

;;; The parser

(defun signal-reader-error (parser)
  (let ((message (libyaml.parser:error-message parser))
        (line (libyaml.parser:error-line parser))
        (column (libyaml.parser:error-column parser)))
    (error 'yaml.error:parsing-error
           :message message
           :line line
           :column column)))

(defun parse-yaml (input)
  "Parse a YAML string, returning a list of tokens."
  (let ((output (make-array 0 :fill-pointer 0 :adjustable t)))
    (with-parser (parser input)
      (with-event (event)
        (loop do
          ;; Parse the next event, checking for errors
          (if (libyaml.parser:parse parser event)
              ;; Decide what to do with the event
              (let ((type (libyaml.event:event-type event)))
                (flet ((add-to-output (data)
                         (vector-push-extend data output)))
                  (cond
                    ;; Stream events
                    ((eql type :stream-start-event)
                     ;; Do nothing
                     t)
                    ((eql type :stream-end-event)
                     (return-from parse-yaml output))
                    ;; Document events, push them to the output list
                    ((or (eql type :document-start-event)
                         (eql type :document-end-event))
                     (add-to-output (list type)))
                    ;; Alias and scalar event, push the type and data pair to
                    ;; the output list
                    ((eql type :alias-event)
                     (add-to-output
                      (cons type
                            (libyaml.event:event-alias-data event))))
                    ((eql type :scalar-event)
                     (add-to-output
                      (cons type
                            (libyaml.event:event-scalar-data event))))
                    ;; Sequence start and end events
                    ((eql type :sequence-start-event)
                     (add-to-output
                      (cons type
                            (libyaml.event:event-sequence-start-data event))))
                    ((eql type :sequence-end-event)
                     (add-to-output (list type)))
                    ;; Mapping start and end events
                    ((eql type :mapping-start-event)
                     (add-to-output
                      (cons type
                            (libyaml.event:event-mapping-start-data event))))
                    ((eql type :mapping-end-event)
                     (add-to-output (list type)))
                    (t
                     t))))
              ;; Signal an error
              (signal-reader-error parser)))))))

(defun parse-tokens (vector)
  (loop for token across vector do
    (print (first token))
    (destructuring-case token
      ;; Documents
      ((:document-start-event)
       t)
      ((:document-end-event)
       t)
      ;; Alias event
      ((:alias-event &key anchor)
       (print anchor))
      ;; Scalar
      ((:scalar-event &key anchor tag value)
       (print (list anchor tag value)))
      ;; Sequence start event
      ((:sequence-start-event &key anchor tag)
       (print (list anchor tag)))
      ;; Mapping start event
      ((:mapping-start-event &key anchor tag)
       (print (list anchor tag)))
      ;; End events
      ((:sequence-end-event)
       t)
      ((:mapping-end-event)
       t)
      ;; Do nothing
      ((t &rest rest)
       t))))

;;; The public interface

(defgeneric parse (input)
  (:documentation "Parse a YAML string or a pathname to a YAML file into Lisp
 data."))

(defmethod parse ((input string))
  (parse-tokens (parse-yaml input)))

(defmethod parse ((input pathname))
  (parse (uiop:read-file-string input)))
