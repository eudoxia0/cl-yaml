(in-package :cl-user)

(defpackage yaml.parser.extensions
  (:use)
  (:export #:*yaml-tag-converter*)
  (:documentation "Symbols related to parser extensions"))

(defpackage yaml.parser
  (:use :cl :yaml.parser.extensions)
  (:import-from :alexandria
                :destructuring-case)
  (:import-from :libyaml.macros
                :with-parser
                :with-event)
  (:export :parse-string
	   :register-scalar-converter
	   :register-sequence-converter
	   :register-mapping-converter)
  (:documentation "The YAML parser."))
(in-package :yaml.parser)

(defvar *yaml-tag-converter* nil
  "Function to call for unregistered tags.

When non-nil, it should be a function accepting a TAG (string) and a
raw yaml VALUE (string, list or hashtable). The function is called
when no scalar, sequence or mapping converter is registered for the
given tag and should return the value to use instead of VALUE.")

(defvar +scalar-converters+ (make-hash-table :test #'equal))
(defvar +sequence-converters+ (make-hash-table :test #'equal))
(defvar +mapping-converters+ (make-hash-table :test #'equal))

(defun scalar-converter (tag)
  (gethash tag +scalar-converters+))

(defun convert-scalar (string tag &optional (style :plain-scalar-stype))
  (let ((converter (scalar-converter tag)))
    (cond
      (converter (funcall converter string))
      ((and tag *yaml-tag-converter*)
       (funcall *yaml-tag-converter* tag string))
      (t (yaml.scalar:parse-scalar string style)))))

(defun sequence-converter (tag)
  (gethash tag +sequence-converters+))

(defun convert-sequence (list tag)
  (let ((converter (sequence-converter tag)))
    (cond
      (converter (funcall converter list))
      ((and tag *yaml-tag-converter*)
       (funcall *yaml-tag-converter* tag list))
      (t list))))

(defun mapping-converter (tag)
  (gethash tag +mapping-converters+))

(defun convert-mapping (hashtable tag)
  (let ((converter (mapping-converter tag)))
    (cond
      (converter (funcall converter hashtable))
      ((and tag *yaml-tag-converter*)
       (funcall *yaml-tag-converter* tag hashtable))
      (t hashtable))))

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
          (let ((parsing-result (libyaml.parser:parse parser event)))
            (if parsing-result
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
                      ;; the output list. Disabled since they are not supported.
                      #|
                      ((eql type :alias-event)
                       (add-to-output
                        (cons type
                              (libyaml.event:event-alias-data event))))
                      |#
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
                       (add-to-output (list type))))))
                ;; Signal an error
                (signal-reader-error parser))))))))

(defun parse-tokens (vector)
  (let ((contexts (list (list :documents))))
    (loop for token across vector do
      (destructuring-case token
        ;; Documents
        ((:document-start-event)
         (push (list) contexts))
        ((:document-end-event)
         (let ((con (pop contexts)))
           (setf (first contexts)
                 (append (first contexts)
                         con))))
        ;; Alias event
        ;; Disabled since it's not supported
        #|
        ((:alias-event &key anchor)
         (declare (ignore anchor))
         t)
        |#
        ;; Scalar
        ((:scalar-event &key anchor tag value length plain-implicit quoted-implicit style)
         (declare (ignore anchor length plain-implicit quoted-implicit))
         (setf (first contexts)
               (append (first contexts)
                       (list (convert-scalar value tag style)))))
        ;; Sequence start event
        ((:sequence-start-event &key anchor tag implicit style)
         (declare (ignore anchor implicit style))
         (push (list tag) contexts))
        ;; Mapping start event
        ((:mapping-start-event &key anchor tag implicit style)
         (declare (ignore anchor implicit style))
         (push (list tag) contexts))
        ;; End events
        ((:sequence-end-event)
         (destructuring-bind (tag &rest seq) (pop contexts)
           (setf (first contexts)
                 (append (first contexts)
                         (list (convert-sequence seq tag))))))
        ((:mapping-end-event)
         (destructuring-bind (tag &rest plist) (pop contexts)
           (setf (first contexts)
                 (append (first contexts)
                         (list (convert-mapping
				(alexandria:plist-hash-table plist :test #'equalp)
				tag))))))
        ;; Do nothing
        ((t &rest rest)
         (declare (ignore rest)))))
    (first contexts)))

;;; The public interface

(defun register-scalar-converter (tag converter)
  (setf (gethash tag +scalar-converters+) converter))

(defun register-sequence-converter (tag converter)
  (setf (gethash tag +sequence-converters+) converter))

(defun register-mapping-converter (tag converter)
  (setf (gethash tag +mapping-converters+) converter))

(defun parse-string (yaml-string)
  (parse-tokens (parse-yaml yaml-string)))
