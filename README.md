# cl-yaml

[![Build Status](https://travis-ci.org/eudoxia0/cl-yaml.svg?branch=master)](https://travis-ci.org/eudoxia0/cl-yaml)

A YAML parser and emitter.

# Overview

## IEEE Floating Point Support

Common Lisp doesn't natively support the IEEE special floating point values: NaN
(Not a number), positive infinity and negative infinity are unrepresentable in
portable Common Lisp. Since YAML allows documents to include these values, we
have to figure out what to do with them. cl-yaml supports multiple float
strategies.

The default strategy is `:keyword`, which uses keywords to represent these
values. The strategy can be customized by setting the value of
`yaml.float:*float-strategy*` to one of the following keywords:

1. `:error`: The simplest approach, simply signal the condition
   `yaml.error:unsupported-float-value` whenever a NaN or infinity value is
   encountered.

2. `:keyword`: Use keywords to represent the different values, i.e.: `:NaN` for
   NaN, `:+Inf` for positive infinity and `:-Inf` for negative infinity.

3. `:best-effort`: Use implementation-specific values whenever possible, fall
   back on `:keyword` in unsupported implementations. On SBCL and Allegro Common
   Lisp, NaN and infinity can be represented.

# Usage

# License

Copyright (c) 2013-2015 Fernando Borretti

Licensed under the MIT License.
