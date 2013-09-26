(defsystem cl-yaml-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-yaml
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "cl-yaml"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
