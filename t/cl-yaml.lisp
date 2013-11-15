(defpackage yaml-test
  (:use :cl :fiveam)
  (:import-from :yaml
                :parse
                :emit))
(in-package :yaml-test)

(def-suite yaml
  :description "General unit tests.")

(test finishes
  (finishes
    (parse "foo")
    (parse "bar")
    (parse "[1,2,3]")
    (parse "[[a,b],[c,d]]")
    (parse "{a : 1, b : 2, c : 3}")
    (parse "{first : [1,2,{a : 1, b : 2}], second: {fst : a, snd : b}}")))

(test flat-int
  (is (equal (parse "[1,2,3]") (list 1 2 3))))

(test flat-str
  (is (equal (parse "[\"foo\",\"bar\"]") (list "foo" "bar"))))

(test nested
  (is (equal (parse "[[a,1],[b,2],[c,3]]")
             (list (list "a" 1)
                   (list "b" 2)
                   (list "c" 3)))))

(test flat-map
  (is (equalp (parse "{a : 1, b : 2}")
              (let ((map (make-hash-table :test #'equal)))
                (setf (gethash "a" map) 1
                      (gethash "b" map) 2)
                map))))

(run!)
