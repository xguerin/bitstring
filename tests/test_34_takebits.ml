(* Test takebits call.
 * $Id$
 *)

open Printf
open Bitstring

let () =
  let bits = make_bitstring 65 '\x5a' in
  for len = 0 to 65 do
    let sub = takebits len bits in
    assert (bitstring_length sub = len)
  done
