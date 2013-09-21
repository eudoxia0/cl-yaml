(defpackage :yaml
  (:use :cl :cffi :split-sequence)
  (:import-from :libyaml
                :yaml-parser-t
                :yaml-parser-t.error
                :yaml-parser-initialize
                :yaml-parser-delete
                :yaml-parser-set-input-string
                :yaml-parser-parse
                :yaml-event-t
                :yaml-event-t.type
                :yaml-event-t.data.scalar.value
                :yaml-event-t.data.scalar.anchor
                :yaml-event-t.data.alias.anchor
                :yaml-event-delete
                ;; Constants
                :+yaml-document-start-event+
                :+yaml-document-end-event+
                :+yaml-stream-start-event+
                :+yaml-stream-end-event+
                :+yaml-scalar-event+
                :+yaml-alias-event+
                :+yaml-sequence-start-event+
                :+yaml-sequence-end-event+
                :+yaml-mapping-start-event+
                :+yaml-mapping-end-event+)
  (:import-from :autowrap
                :alloc)
  (:export :parse
           :emit))
(in-package :yaml)

(define-foreign-library libyaml
  (:unix "libyaml.so")
  (t (:default "libyaml")))

(cffi:use-foreign-library libyaml)

(defmacro with-many-alloc ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (bind) `(,(car bind) (alloc ,(cadr bind))))
          bindings)
     (unwind-protect (progn ,@body)
       ,@(mapcar #'(lambda (bind) `(cffi-sys:foreign-free (autowrap:ptr ,(car bind))))
         bindings)
       ,@(mapcar #'(lambda (bind) `(autowrap:invalidate ,(car bind)))
         bindings))))

(defmacro with-parser (&rest body)
  `(with-many-alloc ((parser 'yaml-parser-t)
                     (event 'yaml-event-t))
    (unwind-protect
      (if (eq (yaml-parser-initialize parser) 1)
          (progn
            ,@body)
          (error "Failed to initialize YAML parser."))
      (yaml-parser-delete parser))))

(defun group-documents (tokens)
  (split-sequence-if
    #'(lambda (elem)
      (or (eq (token-value elem) +yaml-document-end-event+)
          (eq (token-value elem) +yaml-document-start-event+)))
    tokens))

(defun clean (documents)
  "I am not a clever man."
  (remove-if
    #'(lambda (doc)
        (or (null doc)
	    (if (and (listp doc) (null (cdr doc)) (typep (car doc) 'token))
		(let ((tok (token-value (car doc))))
		  (or (null tok)
		      (equal tok +yaml-stream-start-event+)
		      (equal tok +yaml-stream-end-event+)))
		nil)))
    documents))

(defstruct (token (:print-function (lambda (tok stream k)
				     (format stream "~S" (token-value tok)))))
  (anchor :string)
  (value (or :string :fixnum)))

(defun tokenize (str &optional (len (length str)))
  (with-parser
    (yaml-parser-set-input-string parser str len)
    (loop while (not (eq (yaml-event-t.type event) +yaml-stream-end-event+))
      collecting
      (if (eq (yaml-parser-parse parser event) 0)
          (error "Parsing error: ~a" (yaml-parser-t.error parser))
          (let ((type (yaml-event-t.type event)))
	    (prog1
              (cond
                ((eq type +yaml-scalar-event+)
                  (make-token :value (foreign-string-to-lisp (yaml-event-t.data.scalar.value event))
			      :anchor (foreign-string-to-lisp (yaml-event-t.data.scalar.anchor event))))
                ((eq type +yaml-alias-event+)
                  (make-token :value (foreign-string-to-lisp (yaml-event-t.data.alias.anchor event))
			      :anchor nil))
                (t
                  (make-token :value type :anchor nil)))
              (when (not (eq type +yaml-stream-end-event+))
                    (yaml-event-delete event)))))
      into tokens
      finally (progn
                (yaml-event-delete event)
                (return (clean (group-documents tokens)))))))

(defmacro with-string-input (str &optional len)
  (let ((str-name (gensym)))
    `(let ((,str-name ,str))
       (with-foreign-string (s ,str-name)
	 (tokenize s (if ,len ,len (length ,str-name)))))))

(defmacro with-preserved-case (&rest code)
  `(unwind-protect
     (progn
       (setf (readtable-case *readtable*) :preserve)
       ,@code)
     (setf (readtable-case *readtable*) :upcase)))

(defun extract-type (val)
  (handler-case
    (let ((res
	   (if (position #\Space val)
	       val
	       (with-preserved-case
	         (read-from-string val)))))
      (if (symbolp res)
	  (symbol-name res)
	  res))
    (error () val)))

(defun parse% (documents)
  (loop for tokens in documents collecting
    (let ((contexts (list nil)))
      (loop for tok in tokens do
        (let ((val (token-value tok)))
	  (cond
	    ((eql val +yaml-sequence-start-event+)
	     (push (list) contexts))
	    ((eql val +yaml-sequence-end-event+)
	     (let ((con (pop contexts)))
	       (setf (first contexts) (append (first contexts) (list con)))))
	    ((eql val +yaml-mapping-start-event+)
	     (push (list) contexts))
	    ((eql val +yaml-mapping-end-event+)
	     (let ((con (pop contexts)))
	       (setf (first contexts)(append (first contexts)
					     (list (alexandria:plist-hash-table con :test #'equal))))))
	    (t
	     (setf (first contexts) (append (first contexts) (list (extract-type val))))))))
    (caar contexts))))

(defun post-process (documents)
  (if (cdr documents)
      (mapcar #'(lambda (doc) (append (list :doc) doc)) documents)
      (car documents)))

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun parse (src)
  (typecase src
    (string
      (post-process (parse% (with-string-input src))))
    (pathname
      ;; Path
      (post-process
        (parse%
          (with-string-input
	    (with-open-file (stream src :direction :input :if-does-not-exist :error)
	      (slurp-stream stream))))))
    (t
      (error "Unknown input to yaml:load."))))

(defun emit (obj)
  "")
