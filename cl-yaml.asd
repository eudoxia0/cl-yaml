(defsystem cl-yaml
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cffi
               :cl-autowrap
               :split-sequence)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "yaml")
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
