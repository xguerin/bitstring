(* Test if bitstrings are all zeroes or all ones.
 * $Id$
 *)

open Printf

let () =
  for i = 0 to 33 do
    let bits = Bitstring.zeroes_bitstring i in
    if not (Bitstring.is_zeroes_bitstring bits) then (
      eprintf "is_zeros_bitstring failed %d\n" i;
      exit 1
    );
    if i > 0 && Bitstring.is_ones_bitstring bits then (
      eprintf "false match is_ones_bitstring %d\n" i;
      exit 1
    )
  done;
  for i = 0 to 33 do
    let bits = Bitstring.ones_bitstring i in
    if not (Bitstring.is_ones_bitstring bits) then (
      eprintf "is_ones_bitstring failed %d\n" i;
      exit 1
    );
    if i > 0 && Bitstring.is_zeroes_bitstring bits then (
      eprintf "false match is_zeroes_bitstring %d\n" i;
      exit 1
    )
  done
