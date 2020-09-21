(lang dune 2.5)

(name bitstring)

(version 4.0.0)

(generate_opam_files true)

(source (github xguerin/bitstring))
(license LGPL-2.0-or-later)
(authors "Richard W.M. Jones" "Xavier R. Guérin")
(maintainers "Xavier R. Guérin <github@applepine.org>")

(package
 (name bitstring)
 (synopsis "Bitstrings and bitstring matching for OCaml")
 (description
  "\| The ocaml-bitstring project adds Erlang-style bitstrings and matching over bitstrings as a syntax extension and library for OCaml. 
  "\| You can use this module to both parse and generate binary formats, files and protocols. 
  "\| Bitstring handling is added as primitives to the language, making it exceptionally simple to use and very powerful.
 )
 (depends
  (dune (>= 2.5))
  (ocaml (>= 4.04.1))
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
  (ocaml (>= 4.04.1))
  (bitstring (>= 4.0.0))
  (ocaml (and :with-test (>= 4.08.0)))
  (ppxlib (and :build (>= 0.16.0)))
  (ounit :with-test)))
