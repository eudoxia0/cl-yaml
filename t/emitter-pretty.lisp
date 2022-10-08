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
      (is (equal (yaml:emit-pretty-as-document-to-string
                   t)
                 (format
                   nil
                   (concatenate
                     'string
                     "--- ~A~&"
                     "...~&")
                   "true"
                   )))
      (is (equal (yaml:emit-pretty-as-document-to-string
                   nil)
                 (format
                   nil
                   (concatenate
                     'string
                     "--- ~A~&"
                     "...~&")
                   "false"
                   ))))
(test scalar-values
      (loop for a in '("a" "b" 12.5 5) do
      (is (equal (yaml:emit-pretty-as-document-to-string
                   a)
                 (format
                   nil
                   (concatenate
                     'string
                     "--- ~A~&"
                     "...~&")
                   a
                   )))))
(test inverses
      (loop for a in '(t nil "a" "b" 12.5 5) do
      (is (equal
            (cl-yaml:parse (cl-yaml:emit-pretty-as-document-to-string "a"))
            "a"))))

(test doc-example
      (is (equal
            (cl-yaml:emit-pretty-as-document-to-string
              (alexandria:plist-hash-table '("a" t "b" 2.0 "moreducks" (c d e f))))
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