(* Just check that we can run some functions from the library.
 * $Id$
 *)

let () =
  let bits = Bitstring.create_bitstring 16 in
  ignore (Bitstring.string_of_bitstring bits)
