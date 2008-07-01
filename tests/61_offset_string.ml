(* Offset string.  The rotation functions used for strings are
 * very complicated so this is worth testing separately.
 * $Id: 50_named_pattern.ml 86 2008-06-13 15:56:04Z richard.wm.jones $
 *)

open Printf
open Bitmatch

let make_bits si n sj m sk = (
  let pad1 = ones_bitstring (n-64) in
  let pad2 = ones_bitstring (m-n-8) in
  BITSTRING {
    si : 64 : string;
    pad1 : n-64 : bitstring;
    sj : 8 : string;		     (* this should be at offset(n) *)
    pad2 : m-n-8 : bitstring;
    sk : 64 : string		     (* this should be at offset(m) *)
  }
)

let test_bits bits si n sj m sk =
  bitmatch bits with
  | { si' : 64 : string;
      sj' : 8 : string, offset(n);
      sk' : 64 : string, offset(m) }
      when si = si' && sj = sj' && sk = sk' -> () (* ok *)
  | { _ } ->
      failwith (sprintf "61_offset_string: test_bits: failed %S %d %S %d %S"
		  si n sj m sk)

let () =
  for n = 64 to 128 do
    for m = n+8 to 256 do
      List.iter (fun (si,sj,sk) ->
		   test_bits (make_bits si n sj m sk) si n sj m sk)
	["ABCDEFGH", "x", "HGFEDCBA";
	 "01234567", "0", "76543210";
	 "abcdefgh", "\x55", "poiuytre"]
    done;
  done
