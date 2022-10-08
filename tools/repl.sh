#/bin/sh

# Completion file taken from here:
# https://web.archive.org/web/20031207221537/http://weitz.de/files/cmucl_completions
# Pointed out from here: https://stackoverflow.com/a/60281483
# Thanks to Dr. Edi Weitz, who first created the completion file.
# His home page is at https://weitz.de/
# His CL projects are here: https://edicl.github.io/

export RLWRAP_HOME=${PWD}/.repl
export C_INCLUDE_PATH=/usr/include
export LIBRARY_PATH=/usr/lib64:/usr/lib

rlwrap \
    --break-chars "(){}[],'#\";|\\" \
    --case-insensitive \
    --file ./tools/edi-weitz \
    --history-filename ~/.cl-history \
    --forget-matching '^[^(]' \
    --forget-matching '[^)] *$' \
    -M '.lisp' \
    --multi-line='  ' \
    --only-cook '^[^[:blank:]]* *' \
    --prompt-colour=red \
    --quote-characters "\"" \
    --remember \
    ros run -- --load tools/preload.lisp