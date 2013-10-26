(defpackage :libyaml
  (:use :cl :cffi)
  (:export :with-string-input))
(in-package :libyaml)

(load-foreign-library
  (namestring
    (make-pathname :name "yaml_wrapper"
                   :type #+unix "so" #+darwin "dylib" #+windows "dll"
                   :defaults (asdf::component-relative-pathname
                               (asdf:find-system :cl-yaml)))))

(defcfun ("tokenize" tokenize) :pointer (str :string) (len :int))

(defmacro with-string-input (str &optional len)
  (let ((str-name (gensym)))
    `(let ((,str-name ,str))
       (with-foreign-string (s ,str-name)
	 (tokenize s (if ,len ,len (length ,str-name)))))))

(defcfun ("get_len" get-len) :int (docs :pointer))
(defcfun ("get_nth_doc" get-nth-doc) :pointer (docs :pointer) (n :int))

(defcfun ("get_nth_tok" get-nth-tok) :pointer (docs :pointer) (n :int))

(defcfun ("get_type" get-type) :int (tok :pointer))
(defcfun ("get_anchor" get-anchor) :string (tok :pointer))
(defcfun ("get_anchor" get-value) :string (tok :pointer))
