(defpackage :libyaml)
(in-package :libyaml)

(autowrap:c-include "/usr/include/yaml.h"
  :exclude-sources ("/usr/include/bits/waitstatus.h"
                    "/usr/include/stdio.h"
                    "/usr/include/libio.h")
  :spec-path '(cl-yaml "spec"))
