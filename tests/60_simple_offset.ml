(* Simple offset test
 * $Id$
 *)

open Printf
open Bitstring

let make_bits i n j m k = (
  let pad1 = ones_bitstring (n-8) in
  let pad2 = ones_bitstring (m-n-8) in
  BITSTRING {
    i : 8;
    pad1 : n-8 : bitstring;
    j : 8;			     (* this should be at offset(n) *)
    pad2 : m-n-8 : bitstring;
    k : 8			     (* this should be at offset(m) *)
  }
)

let test_bits bits i n j m k =
  bitmatch bits with
  | { i' : 8;
      j' : 8 : offset(n);
      k' : 8 : offset(m) } when i = i' && j = j' && k = k' -> () (* ok *)
  | { _ } ->
      failwith (sprintf "60_simple_offset: test_bits: failed %d %d %d %d %d"
		  i n j m k)

let () =
  for n = 8 to 128 do
    for m = n+8 to 256 do
      List.iter (fun (i,j,k) -> test_bits (make_bits i n j m k) i n j m k)
	[0x55, 0xaa, 0x33; 0x33, 0xaa, 0x55; 0x12, 0x34, 0x56]
    done;
  done
