`cl-yaml` is a [libyaml](http://pyyaml.org/wiki/LibYAML)-based YAML parser for Common Lisp.

# Usage

```lisp
CL-YAML> (yaml:parse #p"network/hosts.yaml")
{"prod" => ("something.herokuapp.com" 6767), "db" => ("somewhere.org" 5432)}
CL-YAML> (yaml:parse "{arch: x86-64, cc: clang, user: eudoxia}")
{"arch" => "x86-64", "cc" => "clang", "user" => "eudoxia"}
CL-USER> (yaml:emit (list "foo" "bar"))
"[\"foo\", \"bar\"]"
CL-USER> (yaml:emit '((a 1) (b 2) (c 3)))
"[[A, 1], [B, 2], [C, 3]]"
```

[Hash table syntax](http://frank.kank.net/essays/hash.html) is used in the examples. I recommend at least using its hash table printer because the default is basically useless.

# Installation

You need `libyaml` for this to work.

# License

Copyright (C) 2013 Fernando Borretti

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
