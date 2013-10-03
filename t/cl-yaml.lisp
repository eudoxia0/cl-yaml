(defpackage yaml-test
  (:use :cl
        :fiveam
        :cffi))
(in-package :yaml-test)

(def-suite list
  :description "Parsing lists")

(test (flat-int nil list)
  (is (equal (yaml:parse "[1,2,3]") (list 1 2 3))))

(test (flat-str nil list)
  (is (equal (yaml:parse "[\"foo\",\"bar\"]") (list "foo" "bar"))))

(test (nested nil list)
  (is (equal (yaml:parse "[[a,1],[b,2],[c,3]]") (list (list "a" 1)
						      (list "b" 2)
						      (list "c" 3)))))

(def-suite map
  :description "Parsing maps")

(test (flat-map nil map)
  (is (equal (yaml:parse "{a : 1, b : 2, c : 3}")
	     (let ((hash (make-hash-table :test #'equalp)))
	       (setf (gethash "a" hash) 1
		     (gethash "b" hash) 2
		     (gethash "c" hash) 3)
	       hash))))

(run!)
