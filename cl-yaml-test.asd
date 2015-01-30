(defsystem cl-yaml-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:cl-yaml
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "parser")
                 (:file "cl-yaml")))))
