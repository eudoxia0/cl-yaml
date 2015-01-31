(in-package :cl-user)
(defpackage cl-yaml-test.scalar
  (:use :cl :fiveam)
  (:export :scalar))
(in-package :cl-yaml-test.scalar)

(def-suite scalar
  :description "YAML scalar parsing tests.")
(in-suite scalar)

(test special-constants
  (is (equal (yaml.scalar:parse-scalar "null")
             nil))
  (is (equal (yaml.scalar:parse-scalar "true")
             t))
  (is (equal (yaml.scalar:parse-scalar "false")
             nil))
  (is (equal (yaml.scalar:parse-scalar "123")
             123))
  (is (equal (yaml.scalar:parse-scalar "012345")
             12345))
  (is (equal (yaml.scalar:parse-scalar "-555")
             -555))
  (is (equal (yaml.scalar:parse-scalar "1.234")
             1.234))
  (is (equal (yaml.scalar:parse-scalar "1e5")
             1e5)))
