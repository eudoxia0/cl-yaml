(defsystem cl-yaml
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/eudoxia0/cl-yaml"
  :bug-tracker "https://github.com/eudoxia0/cl-yaml/issues"
  :source-control (:git "git@github.com:eudoxia0/cl-yaml.git")
  :depends-on (:cl-libyaml)
  :components ((:module "src"
                :serial t
                :components
                ((:file "error")
                 (:file "parser")
                 (:file "emitter"))))
  :description "A YAML parser and emitter."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-yaml-test))))
