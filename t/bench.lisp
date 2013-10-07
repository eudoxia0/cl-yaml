(in-package :yaml-test)

(defun make-gen ()
  (lambda () (random 99)))

(defun generate-list (gen &optional (len 100))
  (yaml:emit (loop repeat len collecting (funcall gen))))

(defun benchmark (str)
  (format t "Bytes: ~A~&Time to parse:~&" (length str))
  (time (yaml:parse str))
  (terpri))

(defun benchmark-len (n)
  (benchmark (generate-list (make-gen) n)))

(defun run-benchmarks ()
  (let ((gen (make-gen)))
    (loop for i from 1 to 256 do
	 (benchmark (generate-list (* 64 i))))))

(export 'benchmark-len)
(export 'run-benchmarks)
