(in-package :cl-user)
(defpackage cl-yaml-test.parser
  (:use :cl :fiveam)
  (:export :parser))
(in-package :cl-yaml-test.parser)

(def-suite parser
  :description "YAML parser tests.")
(in-suite parser)

(test lists
  (is
   (equal (yaml.parser:parse "[a, b, c]")
          (list :document (list "a" "b" "c")))))
