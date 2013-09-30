(in-package :cl-yaml-test)

(defun make-gen ()
  (lambda () (random 99)))

(defun generate-list (gen &optional (len 100))
  (yaml:emit (loop repeat len collecting (funcall gen))))

(defun benchmark (str)
  (format t "Bytes: ~A~&Time to parse:~&" (length str))
  (time (yaml:parse str))
  (terpri))

(defun run-benchmarks ()
  (loop for i from 1 to 256 do
       (benchmark (generate-list (make-gen) (* 256 i)))))
