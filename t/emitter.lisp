(in-package :cl-user)
(defpackage cl-yaml-test.emitter
  (:use :cl :fiveam)
  (:export :emitter))
(in-package :cl-yaml-test.emitter)

;;; Macros

(defmacro define-test-cases ((name) &rest pairs)
  `(test ,name
     ,@(loop for (form string) in pairs collecting
         `(is (equal (yaml.emitter:emit-to-string ,form)
                     ,string)))))

;;; Tests

(def-suite emitter
  :description "YAML emitter tests.")
(in-suite emitter)

(define-test-cases (boolean)
  (t
   "true")
  (nil
   "false"))

(define-test-cases (integers)
  (1
   "1")
  (123
   "123")
  (+123
   "123")
  (-123
   "-123"))

(define-test-cases (floats)
  (1.23
   "1.23")
  (6.62607e-34
   "6.62607e-34"))
