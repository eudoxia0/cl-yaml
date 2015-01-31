(defsystem cl-yaml-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:cl-yaml
               :fiveam
               :alexandria)
  :components ((:module "t"
                :serial t
                :components
                ((:file "float")
                 (:file "scalar")
                 (:file "parser")
                 (:file "emitter")
                 (:file "cl-yaml")))))
