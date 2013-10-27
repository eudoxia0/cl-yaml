(in-package :cl-user)
(defpackage cl-yaml-asd
  (:use :cl :asdf))
(in-package :cl-yaml-asd)

(defclass c->so (source-file) ())

(defmethod source-file-type ((c c->so) (s module)) "c")

(defmethod output-files ((operation compile-op) (f c->so))
  (values
    (list
      (make-pathname :name "yaml_wrapper"
                     :type #+unix "so" #+darwin "dylib" #+windows "dll"
                     :defaults
                     (merge-pathnames
                      (make-pathname :directory '(:relative :up))
                      (component-pathname f)))) t))

(defmethod perform ((o load-op) (c c->so)) t)

(defparameter +c-flags+ "-Wall -Wextra -c -fPIC -O0 -ansi")
(defparameter +linker-flags+ "-lyaml")

(defun comp (file out)
  (format t "cc ~A -o out.o ~A && cc out.o -shared -o ~A ~A && rm out.o"
          (namestring file) +c-flags+ (namestring out) +linker-flags+)
  (format nil "cc ~A -o out.o ~A && cc out.o -shared -o ~A ~A && rm out.o"
          (namestring file) +c-flags+ (namestring out) +linker-flags+))

(defmethod perform ((o compile-op) (c c->so))
  (if (not (zerop (run-shell-command
                   (comp (make-pathname :name "yaml"
                                        :type "c"
                                        :defaults
                                        (merge-pathnames
                                         "src"
                                         (component-pathname c)))
                         (make-pathname :name "yaml_wrapper"
                                        :type "so"
                                        :defaults
                                        (merge-pathnames
                                         (make-pathname :directory '(:relative :up))
                                         (component-pathname c)))))))
      (error 'operation-error :component c :operation o)
      t))

(defsystem cl-yaml
  :version "0.2"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cffi
               :split-sequence)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:static-file "yaml.h")
                 (c->so "yaml" :depends-on ("yaml.h"))
                 (:file "ffi")
                 (:file "cl-yaml")))
               (:module "spec"))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-yaml-test))))
