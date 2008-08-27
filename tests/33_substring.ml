(* Test subbitstring call.
 * $Id$
 *)

open Printf
open Bitstring

let () =
  let bits = make_bitstring 65 '\x5a' in
  for off = 0 to 65 do
    for len = 65-off to 0 do
      let sub = subbitstring bits off len in
      for i = 0 to len-1 do
	if get bits (off+i) <> get sub i then (
	  eprintf "33_substring: failed %d %d %d\n" off len i;
	  exit 1
	)
      done
    done
  done
