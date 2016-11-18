(in-package :cl-user)
(defpackage cl-yaml-test.emitter
  (:use :cl :fiveam)
  (:export :emitter)
  (:documentation "Emitter tests."))
(in-package :cl-yaml-test.emitter)

(defmacro define-test-cases ((name) &rest pairs)
  `(test ,name
     ,@(loop for (form string) in pairs collecting
	    `(is (equal (yaml.emitter:with-emitter-to-string (emitter)
			  (yaml.emitter:emit-stream (emitter)
			    (yaml.emitter:emit-document (emitter :implicit t)
			      (yaml.emitter:emit-scalar emitter ,form))))
			,(format nil "~a~%...~%" string)))))) ;; Document end marker
                                                              ;; is libyaml behavior

(defun test-emit-sequence (sequence style)
  (yaml.emitter:with-emitter-to-string (emitter)
    (yaml.emitter:emit-stream (emitter)
      (yaml.emitter:emit-document (emitter :implicit t)
	(yaml.emitter:emit-sequence (emitter :style style)
	  (mapcar (lambda (element) (yaml.emitter:emit-scalar emitter element))
		  sequence))))))

(defun test-emit-mapping (mapping style)
  (yaml.emitter:with-emitter-to-string (emitter)
    (yaml.emitter:emit-stream (emitter)
      (yaml.emitter:emit-document (emitter :implicit t)
	(yaml.emitter:emit-mapping (emitter :style style)
	  (mapcar (lambda (pair)
		    (yaml.emitter:emit-scalar emitter (car pair))
		    (yaml.emitter:emit-scalar emitter (cdr pair)))
		  mapping))))))

(def-suite emitter
    :description "YAML emitter tests.")
(in-suite emitter)

(define-test-cases (boolean)
  (t "true")
  (nil "false"))

(define-test-cases (integers)
  (1 "1")
  (123 "123")
  (+123 "123")
  (-123 "-123"))

(define-test-cases (floats)
  (1.23 "1.23")
  (6.62607e-34 "6.62607e-34"))

(test flow-sequence
  (is (equal (test-emit-sequence '(1 "a" 3.14f0 3.14d0) :flow-sequence-style)
	     "[1, a, 3.14, 3.14]
")))

(test block-sequence
  (is (equal (test-emit-sequence '(1 "a" 3.14f0 3.14d0) :block-sequence-style)
	     "- 1
- a
- 3.14
- 3.14
")))

(test flow-mapping
  (is (equal (test-emit-mapping '(("integer" . 1) ("string" . "test") ("bool" . nil))
				:flow-mapping-style)
	     "{integer: 1, string: test, bool: false}
")))

(test block-mapping
  (is (equal (test-emit-mapping '(("integer" . 1) ("string" . "test") ("bool" . nil))
				:block-mapping-style)
	     "integer: 1
string: test
bool: false
")))

