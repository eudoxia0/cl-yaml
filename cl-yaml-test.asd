(defsystem cl-yaml-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "cl-yaml tests."
  :depends-on (:cl-yaml
               :fiveam
               :alexandria
               :yason
               :generic-comparability
               :cl-fad
               :trivial-benchmark)
  :components ((:module "t"
                :serial t
                :components
                ((:file "float")
                 (:file "scalar")
                 (:file "parser")
                 (:file "emitter")
                 (:file "emitter2")
                 (:file "emitter-pretty")
                 (:file "round-trip")
                 (:file "spec")
                 (:file "bench")
                 (:file "cl-yaml")))))
