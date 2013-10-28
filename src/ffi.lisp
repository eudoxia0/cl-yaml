(defpackage :libyaml
  (:use :cl :cffi)
  (:export :tokenize
           :list-len
           :list-err
           :nth-tok
           :destroy-nth-tok
           :tok-type
           :tok-value
           :tok-anchor
           :destroy-token-list
           :+enum+
           :+scalar+
           :+alias+
           :+seq-start+
           :+seq-end+
           :+map-start+
           :+map-end+
           :+doc-start+
           :+doc-end+
           :+stream-start+
           :+stream-end+))
(in-package :libyaml)

(load-foreign-library
  (namestring
    (make-pathname :name "yaml_wrapper"
                   :type #+unix "so" #+darwin "dylib" #+windows "dll"
                   :defaults (asdf::component-relative-pathname
                               (asdf:find-system :cl-yaml)))))

(defcfun ("tokenize" tokenize) :pointer (str :string) (len :int))

;; Accessors

(defcfun ("list_len" list-len) :int (list :pointer))
(defcfun ("nth_tok" nth-tok) :pointer (list :pointer) (n :int))
(defcfun ("destroy_nth_tok" destroy-nth-tok) :void (list :pointer) (n :int))
(defcfun ("list_err" list-err) :string (list :pointer))
(defcfun ("tok_type" tok-type) :int (tok :pointer))
(defcfun ("tok_value" tok-value) :string (tok :pointer))
(defcfun ("tok_anchor" tok-anchor) :string (tok :pointer))

(defcfun ("destroyTokenList" destroy-token-list) :void (list :pointer))

;; Enum values

(defcfun ("enum_scalar" enum-scalar) :int)
(defcfun ("enum_alias" enum-alias) :int)
(defcfun ("enum_seq_start" enum-seq-start) :int)
(defcfun ("enum_seq_end" enum-seq-end) :int)
(defcfun ("enum_map_start" enum-map-start) :int)
(defcfun ("enum_map_end" enum-map-end) :int)
(defcfun ("enum_doc_start" enum-doc-start) :int)
(defcfun ("enum_doc_end" enum-doc-end) :int)
(defcfun ("enum_stream_start" enum-stream-start) :int)
(defcfun ("enum_stream_end" enum-stream-end) :int)

(defparameter +enum+ (make-hash-table))

(setf (gethash (enum-scalar) +enum+) :scalar)
(setf (gethash (enum-alias) +enum+) :alias)
(setf (gethash (enum-seq-start) +enum+) :seq-start)
(setf (gethash (enum-seq-end) +enum+) :seq-end)
(setf (gethash (enum-map-start) +enum+) :map-start)
(setf (gethash (enum-map-end) +enum+) :map-end)
(setf (gethash (enum-doc-start) +enum+) :doc-start)
(setf (gethash (enum-doc-end) +enum+) :doc-end)
(setf (gethash (enum-stream-start) +enum+) :stream-start)
(setf (gethash (enum-stream-end) +enum+) :stream-end)

(defparameter +scalar+ (enum-scalar))
(defparameter +alias+ (enum-alias))
(defparameter +seq-start+ (enum-seq-start))
(defparameter +seq-end+ (enum-seq-end))
(defparameter +map-start+ (enum-map-start))
(defparameter +map-end+ (enum-map-end))
(defparameter +doc-start+ (enum-doc-start))
(defparameter +doc-end+ (enum-doc-end))
(defparameter +stream-start+ (enum-stream-start))
(defparameter +stream-end+ (enum-stream-end))
