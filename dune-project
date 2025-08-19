(lang dune 2.7)

(name bitstring)

(version 5.0.1)

(generate_opam_files true)

(source (github xguerin/bitstring))
(license LGPL-2.0-or-later)
(authors "Richard W.M. Jones" "Xavier R. Guérin")
(maintainers "Xavier R. Guérin <xguerin@users.noreply.github.com>")

(package
 (name bitstring)
 (synopsis "Bitstrings and bitstring matching for OCaml")
 (description
  "\| The ocaml-bitstring project adds Erlang-style bitstrings and matching over bitstrings as a syntax extension and library for OCaml. 
  "\| You can use this module to both parse and generate binary formats, files and protocols. 
  "\| Bitstring handling is added as primitives to the language, making it exceptionally simple to use and very powerful.
 )
 (depends
  (ocaml (>= 5.2.0))
  (stdlib-shims (>= 0.1.0))
))

(package
 (name ppx_bitstring)
 (synopsis "Bitstrings and bitstring matching for OCaml - PPX extension")
 (description
  "\| The ocaml-bitstring project adds Erlang-style bitstrings and matching over bitstrings as a syntax extension and library for OCaml. 
  "\| You can use this module to both parse and generate binary formats, files and protocols. 
  "\| Bitstring handling is added as primitives to the language, making it exceptionally simple to use and very powerful.
 )
 (depends
  (ocaml (>= 5.2.0))
  (bitstring (>= 5.0.0))
  (ocaml (and :with-test (>= 5.2.0)))
  (ppxlib (>= 0.36.0))
  (ounit :with-test)))
