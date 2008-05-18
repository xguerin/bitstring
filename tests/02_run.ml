(* Just check that we can run some functions from the library.
 * $Id$
 *)

let () =
  let bits = Bitmatch.create_bitstring 16 in
  ignore (Bitmatch.string_of_bitstring bits)
