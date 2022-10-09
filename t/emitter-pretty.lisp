(in-package :cl-user)
(defpackage cl-yaml-test.emitter-pretty
  (:use :cl :fiveam)
  (:export :emitter-pretty)
  (:documentation "Emitter tests - pretty functions."))
(in-package :cl-yaml-test.emitter-pretty)

(def-suite emitter-pretty
    :description "YAML emitter tests - pretty functions.")
(in-suite emitter-pretty)

(defun join-lines (lines)
  (apply (alexandria:curry
           #'concatenate
           'string)
         (mapcar (lambda (lne) (format nil "~A~%" lne))
                 lines)))

(test booleans
      (is (equal (yaml:with-emitter-to-string
                   (em)
                   (yaml:emit-pretty-as-document
                     em t))
                 (format
                   nil
                   (concatenate
                     'string
                     "--- ~A~&"
                     "...~&")
                   "true"
                   )))
      (is (equal (yaml:with-emitter-to-string
                   (em)
                   (yaml:emit-pretty-as-document
                     em nil))
                 (format
                   nil
                   (concatenate
                     'string
                     "--- ~A~&"
                     "...~&")
                   "false"
                   ))))
(test inverses
      (loop for a in '(t nil "a" "b" 12.5 5) do
      (is (equal
            (yaml:parse (yaml:with-emitter-to-string
                          (em)
                          (yaml:emit-pretty-as-document em "a")))
            "a"))))

(test scalar-values
      (loop for a in '("a" "b" 12.5 5) do
      (is (equal (yaml:with-emitter-to-string
                   (em)
                   (yaml:emit-pretty-as-document
                   em a))
                 (format
                   nil
                   (concatenate
                     'string
                     "--- ~A~&"
                     "...~&")
                   a
                   )))))

(test doc-example
      (is (equal
            (yaml:with-emitter-to-string
              (em)
              (yaml:emit-pretty-as-document
                em
                (alexandria:plist-hash-table '("a" t "b" 2.0 "moreducks" (c d e f)))))
            (join-lines
              '(
                "---"
                "a: true"
                "b: 2.0"
                "moreducks:"
                "- C"
                "- D"
                "- E"
                "- F"
                "...")))))
; (fiveam:run! 'emitter-pretty)