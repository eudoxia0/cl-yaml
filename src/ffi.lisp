(defpackage :libyaml
  (:use :cl :cffi))
(in-package :libyaml)

(load-foreign-library
  (namestring
    (make-pathname :name "yaml_wrapper"
                   :type #+unix "so" #+darwin "dylib" #+windows "dll"
                   :defaults (asdf::component-relative-pathname
                               (asdf:find-system :cl-yaml)))))

(defcstruct token
  (anchor :string)
  (value  :string)
  (type   :int))

(defcstruct token-list
  (data :pointer)
  (len  :int)
  (cap  :int))

(defcstruct document-list
  (data :pointer)
  (len  :int)
  (cap  :int)
  (err  :string))

(defcfun tokenize document-list (str :string) (len :int))

(defmacro with-string-input (str &optional len)
  (let ((str-name (gensym)))
    `(let ((,str-name ,str))
       (with-foreign-string (s ,str-name)
	 (tokenize s (if ,len ,len (length ,str-name)))))))
