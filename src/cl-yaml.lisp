(defpackage :yaml
  (:use :cl :split-sequence :libyaml)
  (:import-from :cffi
                :with-foreign-string
                :foreign-string-to-lisp)
  (:export :parse
           :emit))
(in-package :yaml)

(defmacro with-parser ((str len) &rest body)
  `(let ((parser (make-parser ,str ,len))
         (event (make-event)))
    (unwind-protect
      (if parser
          (progn ,@body)
          (error "Could not initialize parser"))
      (delete-parser parser))))

(defstruct (token (:print-function (lambda (tok stream k)
				     (declare (ignore k))
				     (format stream "~S" (token-value tok)))))
  type
  (anchor :string)
  (value (or :string :fixnum)))

(defun group-documents (tokens)
  (split-sequence-if
    #'(lambda (elem)
      (or (eq (token-value elem) +doc-start+)
          (eq (token-value elem) +doc-end+)))
    tokens))

(defun clean (documents)
  "I am not a clever man."
  (remove-if
    #'(lambda (doc)
        (or (null doc)
	    (if (and (listp doc) (null (cdr doc)) (typep (car doc) 'token))
		(let ((tok (token-type (car doc))))
		  (or (null tok)
		      (equal tok +stream-start+)
		      (equal tok +stream-end+)))
		nil)))
    documents))

(defun tokenize (str &optional (len (length str)))
  (with-parser (str len)
    (loop while (not (eq (event-type event) +stream-end+))
      collecting
      (if (eql (parse-event parser event) 0)
          (error "Parse error: ~A" "I have no idea")
          (let ((type (gethash (event-type event) +enum+)))
	    (prog1
              (cond
                ((eq type +scalar+)
                  (make-token
                   :type +scalar+
                   :value (event-value event)
                   :anchor (event-anchor event)))
                ((eq type +alias+)
                  (make-token
                   :type +alias+
                   :value (event-anchor event)
                   :anchor nil))
                (t
                 (make-token
                   :type type
                   :anchor nil)))
              (when (not (eq type +stream-end+))
                    (delete-event event)))))
      into tokens
      finally (progn
                (delete-event event)
                (return (clean (group-documents tokens)))))))

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
    (let ((contexts (list nil))
          (aliases  (make-hash-table :test #'equal)))
      (loop for tok in tokens do
        (let ((type (token-type tok))
              (val (token-value tok)))
	  (cond
            ((if (token-anchor tok)
                 (setf (first contexts)
                       (append (first contexts)
                               (list (gethash val (token-value tok)))))))
	    ((eql type +seq-start+)
	     (push (list) contexts))
	    ((eql type +seq-end+)
	     (let ((con (pop contexts)))
	       (setf (first contexts) (append (first contexts) (list con)))))
	    ((eql type +map-start+)
	     (push (list) contexts))
	    ((eql type +map-end+)
	     (let ((con (pop contexts)))
	       (setf (first contexts) (append (first contexts)
					      (list (alexandria:plist-hash-table con :test #'equal))))))
            ((eql type +alias+)
             (setf (gethash val aliases) (first contexts)))
	    (t
	     (setf (first contexts)
                   (append (first contexts)
                           (list (extract-type (token-value tok)))))))))
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
     (post-process (parse% (tokenize src (length src)))))
    (pathname
     (let ((str (with-open-file
                    (stream src :direction :input :if-does-not-exist :error)
                  (slurp-stream stream))))
       (post-process
        (parse%
         (tokenize str (length str))))))
    (t
      (error "Unknown input to yaml:load."))))

(defun emit (obj)
  (typecase obj
    (number
      (princ-to-string obj))
    (string
      (format nil "~S" obj))
    (symbol
      (format nil "~A" obj))
    (list
      (format nil "[~{~A~#[~:;, ~]~}]" (mapcar #'emit obj)))
    (hash-table
     (format nil "{~{~A~#[~:;, ~]~}}"
	     (loop for key being the hash-keys of obj collecting
		  (format nil "~A : ~A" key (gethash key obj)))))))
