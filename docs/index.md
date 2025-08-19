# Overview

[![Build Status](https://travis-ci.org/xguerin/bitstring.svg?branch=master)](https://travis-ci.org/xguerin/bitstring)
```
Copyright (C) 2008-2016 Red Hat Inc, Richard W.M. Jones.
Copyright (C) 2016-2022 Red Hat Inc, Richard W.M. Jones, Xavier R. Guérin.
```
## How to install
```
opam install bitstring
opam install ppx_bitstring
```
## How to use

### Ocamlfind
```
ocamlfind c -package bitstring -package ppx_bitstring -linkpkg ...
```
### Dune
```lisp
(executable
 ((name        foo)
  (libraries   (bitstring))
  (preprocess  (pps ppx_bitstring))
  ))
```
## How to build

### Building the project
```
$ dune build
```
### Running the tests
```
$ dune runtest
```
## License

The library is licensed under the LGPL v2 or later, with the OCaml linking
exception. See the file `COPYING.LIB` for full terms. Programs are licensed
under the GPL v2 or later. see the file copying for full terms. All examples and
tests are public domain.
