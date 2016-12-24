(in-package :cl-user)
(defpackage yaml.emitter
  (:use :cl)
  (:import-from :cffi
                :foreign-free
                :null-pointer)
  (:import-from :libyaml.emitter
                :allocate-emitter
                :emitter-initialize
                :emitter-delete
                :set-output
                :stream-start-event-initialize
                :stream-end-event-initialize
                :document-start-event-initialize
                :document-end-event-initialize
                :scalar-event-initialize
                :sequence-start-event-initialize
                :sequence-end-event-initialize
                :mapping-start-event-initialize
                :mapping-end-event-initialize)
  (:import-from :libyaml.event
                :allocate-event
                :event-delete)
  (:import-from :libyaml.write-handler
                :*write-handler-callback*
                :*write-handler-stream*)
  (:export ;; Original interface
           :emit
           :emit-to-string
           :encode
           ;; libyaml based interface
           :stream-start-event
           :stream-end-event
           :document-start-event
           :document-end-event
           :scalar-event
           :sequence-start-event
           :sequence-end-event
           :mapping-start-event
           :mapping-end-event
           :emit-stream
           :emit-document
           :emit-sequence
           :emit-mapping
           :emit-scalar
           :emit-object
           :print-scalar
           :with-emitter-to-stream
           :with-emitter-to-string)
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

(defmethod encode ((string string) stream)
  "Encode a string."
  ;; (write-string string stream)
  (format stream "~s" string))

(defmethod encode ((list list) stream)
  "Encode a list."
  (write-string "[" stream)
  (loop for sublist on list do
    (encode (first sublist) stream)
    (when (rest sublist)
      (write-string ", " stream)))
  (write-string "]" stream))

(defmethod encode ((vector vector) stream)
  "Encode a vector."
  (encode (loop for elem across vector collecting elem) stream))

(defmethod encode ((table hash-table) stream)
  "Encode a hash table."
  (write-string "{ " stream)
  (loop for sublist on (alexandria:hash-table-keys table) do
    (let ((key (first sublist)))
      (encode key stream)
      (write-string ": " stream)
      (encode (gethash key table) stream)
      (when (rest sublist)
        (write-string ", " stream))))
  (write-string " }" stream))

;;; Interface

(defun emit (value stream)
  "Emit a value to a stream."
  (encode value stream))

(defun emit-to-string (value)
  "Emit a value to string."
  (with-output-to-string (stream)
(emit value stream)))

;;; Wrappers around cl-libyaml event interface with defaults and keyword args

(defun stream-start-event (event &key (encoding :utf8-encoding))
  (stream-start-event-initialize event encoding))

(defun stream-end-event (event)
  (stream-end-event-initialize event))

(defun document-start-event (event &key (version-directive (null-pointer))
                                     (tag-directive-start (null-pointer))
                                     (tag-directive-end (null-pointer))
                                     (implicit nil))
  (document-start-event-initialize event version-directive
                                   tag-directive-start
                                   tag-directive-end
                                   implicit))

(defun document-end-event (event &key (implicit nil))
  (document-end-event-initialize event implicit))

(defun sequence-start-event (event &key (anchor (null-pointer))
                                     (tag (null-pointer))
                                     (implicit nil)
                                     (style :any-sequence-style))
  (sequence-start-event-initialize event anchor tag implicit style))

(defun sequence-end-event (event)
  (sequence-end-event-initialize event))

(defun mapping-start-event (event &key (anchor (null-pointer))
                                    (tag (null-pointer))
                                    (implicit nil)
                                    (style :any-mapping-style))
  (mapping-start-event-initialize event anchor tag implicit style))

(defun mapping-end-event (event)
  (mapping-end-event-initialize event))

(defun scalar-event (event value length &key (anchor (null-pointer))
                                          (tag (null-pointer))
                                          (plain-implicit t)
                                          (quoted-implicit t)
                                          (style :plain-scalar-style))
  (scalar-event-initialize event anchor tag value length
                           plain-implicit quoted-implicit style))

;;; Emitter macros and output functions

;;; When passing a foreign emitter object, it is also paired with a
;;; foreign event object.

(defun foreign-emitter (emitter) (car emitter))

(defun foreign-event (emitter) (cdr emitter))

(defmacro with-emitter-to-stream ((emitter-var output-stream) &rest body)
  (let ((foreign-emitter (gensym "EMITTER"))
        (foreign-event (gensym "EVENT")))
    `(let* ((,foreign-emitter (allocate-emitter))
            (,foreign-event (allocate-event))
            (,emitter-var (cons ,foreign-emitter ,foreign-event))
            (*write-handler-stream* ,output-stream))
      (unwind-protect
           (progn
             (emitter-initialize ,foreign-emitter)
             (set-output ,foreign-emitter *write-handler-callback* (null-pointer))
             ,@body)
        (libyaml.event:event-delete ,foreign-event)
        (libyaml.emitter:emitter-delete ,foreign-emitter)
        (foreign-free ,foreign-event)
        (foreign-free ,foreign-emitter)))))

(defmacro with-emitter-to-string ((emitter-var) &rest body)
  (let ((str (gensym "STR")))
    `(with-output-to-string (,str)
       (with-emitter-to-stream (,emitter-var ,str)
         ,@body))))

(defmacro emit-stream ((emitter &key (encoding :utf8-encoding)) &body body)
  (let ((emitter-value (gensym "EMITTER"))
        (foreign-emitter (gensym "FOREIGN-EMITTER"))
        (foreign-event (gensym "FOREIGN-EVENT")))
    `(let* ((,emitter-value ,emitter)
            (,foreign-emitter (foreign-emitter ,emitter-value))
            (,foreign-event (foreign-event ,emitter-value)))
       (stream-start-event ,foreign-event :encoding ,encoding)
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event)
       ,@body
       (stream-end-event ,foreign-event)
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event))))

(defmacro emit-document ((emitter &rest rest
                                  &key version-directive
                                  tag-directive-start 
                                  tag-directive-end
                                  (implicit nil)) &body body)
  (declare (ignorable version-directive tag-directive-start
                      tag-directive-end implicit))
  (let ((emitter-value (gensym "EMITTER"))
        (foreign-emitter (gensym "FOREIGN-EMITTER"))
        (foreign-event (gensym "FOREIGN-EVENT")))
    `(let* ((,emitter-value ,emitter)
            (,foreign-emitter (foreign-emitter ,emitter-value))
            (,foreign-event (foreign-event ,emitter-value)))
       (apply #'document-start-event ,foreign-event (list ,@rest))
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event)
       ,@body
       (document-end-event ,foreign-event :implicit ,implicit)
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event))))

(defmacro emit-mapping ((emitter &rest rest &key anchor tag implicit style)
                        &body body)
  (declare (ignorable anchor tag implicit style))
  (let ((emitter-value (gensym "EMITTER"))
        (foreign-emitter (gensym "FOREIGN-EMITTER"))
        (foreign-event (gensym "FOREIGN-EVENT")))
    `(let* ((,emitter-value ,emitter)
            (,foreign-emitter (foreign-emitter ,emitter-value))
            (,foreign-event (foreign-event ,emitter-value)))
       (apply #'mapping-start-event ,foreign-event (list ,@rest))
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event)
       ,@body
       (mapping-end-event ,foreign-event)
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event))))

(defmacro emit-sequence ((emitter &rest rest &key anchor tag implicit style)
                         &body body)
  (declare (ignorable anchor tag implicit style))
  (let ((emitter-value (gensym "EMITTER"))
        (foreign-emitter (gensym "FOREIGN-EMITTER"))
        (foreign-event (gensym "FOREIGN-EVENT")))
    `(let* ((,emitter-value ,emitter)
            (,foreign-emitter (foreign-emitter ,emitter-value))
            (,foreign-event (foreign-event ,emitter-value)))
       (apply #'sequence-start-event ,foreign-event (list ,@rest))
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event)
       ,@body
       (sequence-end-event ,foreign-event)
       (libyaml.emitter:emit ,foreign-emitter ,foreign-event))))

(defun emit-scalar (emitter value &rest rest &key anchor tag
                                               plain-implicit
                                               quoted-implicit
                                               style)
  (declare (ignorable anchor tag plain-implicit quoted-implicit style))
  (let ((printed-value (print-scalar value)))
    (apply #'scalar-event (foreign-event emitter)
           printed-value (length printed-value) rest)
    (libyaml.emitter:emit (foreign-emitter emitter) (foreign-event emitter))))

(defgeneric print-scalar (scalar)
  (:documentation "Convert a scalar object into its printed representation"))

(defmethod print-scalar ((scalar (eql 't)))
  "true")

(defmethod print-scalar ((scalar (eql 'nil)))
  "false")

(defmethod print-scalar ((scalar symbol))
  (symbol-name scalar))

(defmethod print-scalar ((scalar string))
  scalar)

(defmethod print-scalar ((scalar integer))
  (princ-to-string scalar))

(defmethod print-scalar ((scalar single-float))
  (let ((*read-default-float-format* 'single-float))
    (princ-to-string scalar)))

(defmethod print-scalar ((scalar double-float))
  (let ((*read-default-float-format* 'double-float))
    (princ-to-string scalar)))

(defgeneric emit-object (emitter obj)
  (:documentation "Emit YAML representation of obj"))

(defmethod emit-object (emitter (obj symbol))
  (emit-scalar emitter obj))

(defmethod emit-object (emitter (obj string))
  (emit-scalar emitter obj))

(defmethod emit-object (emitter (obj integer))
  (emit-scalar emitter obj))

(defmethod emit-object (emitter (obj float))
  (emit-scalar emitter obj))

