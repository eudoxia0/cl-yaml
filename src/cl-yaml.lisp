(defpackage :yaml
  (:use :cl :split-sequence :libyaml)
  (:import-from :cffi
                :with-foreign-string
                :foreign-string-to-lisp)
  (:export :parse
           :emit))
(in-package :yaml)

(defun clean (tokens)
  "I am not a clever man."
  (remove-if
   #'(lambda (tok)
       (or (eq (first tok) :stream-start)
           (eq (first tok) :stream-end)))
   tokens))

(defun group-documents (tokens)
  (remove-if #'(lambda (seq) (eql (length seq) 0))
             (split-sequence-if
              #'(lambda (tok)
                  (or (eq (first tok) :doc-start)
                      (eq (first tok) :doc-end)))
              tokens)))

(defun process (str len)
  (let ((tok-list (tokenize str len))
        (tokens (make-array 64 :fill-pointer 0)))
    (if (list-err tok-list)
        (error "Parsing error")
        (progn
          (loop for i from 0 to (list-len tok-list) do
            (let* ((tok (nth-tok tok-list i))
                   (type (gethash (tok-type tok) +enum+)))
              (if type
                  (vector-push (list type
                                     (tok-value tok)
                                     (tok-anchor tok))
                               tokens))))
          (group-documents (clean tokens))))))

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
      (loop for tok across tokens do
        (let ((type   (first tok))
              (val    (second tok))
              (anchor (third tok)))
	  (cond
	    ((eq type :seq-start)
	     (push (list) contexts))
	    ((eq type :seq-end)
	     (let ((con (pop contexts)))
	       (setf (first contexts) (append (first contexts) (list con)))))
	    ((eq type :map-start)
	     (push (list) contexts))
	    ((eq type :map-end)
	     (let ((con (pop contexts)))
	       (setf (first contexts)
                     (append (first contexts)
                             (list
                              (alexandria:plist-hash-table con :test #'equal))))))
            ((eq type :alias)
             (setf (gethash val aliases) (first contexts)))
            (anchor
             (setf (first contexts)
                   (append (first contexts)
                           (list (gethash val aliases)))))
	    (t
	     (setf (first contexts)
                   (append (first contexts)
                           (list (extract-type val))))))))
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
     (post-process (parse% (process src (length src)))))
    (pathname
     (let ((str (with-open-file
                    (stream src :direction :input :if-does-not-exist :error)
                  (slurp-stream stream))))
       (post-process
        (parse%
         (process str (length str))))))
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
