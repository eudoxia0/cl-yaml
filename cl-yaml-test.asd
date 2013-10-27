(defsystem cl-yaml-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-yaml
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "cl-yaml")
		 (:file "bench"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
