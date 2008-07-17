(* Test save_offset_to.
 * $Id$
 *)

open Printf
open Bitmatch

let make_bits p i n j m k = (
  let pad0 = ones_bitstring p in
  let pad1 = ones_bitstring (n-8) in
  let pad2 = ones_bitstring (m-n-8) in
  BITSTRING {
    pad0 : p : bitstring;	     (* will be skipped below *)
    i : 8;
    pad1 : n-8 : bitstring;
    j : 8;			     (* this should be at offset(n) *)
    pad2 : m-n-8 : bitstring;
    k : 8			     (* this should be at offset(m) *)
  }
)

let test_bits bits p i n j m k =
  (* Skip the 'p' padding bits so the match starts at a non-zero offset. *)
  let bits = dropbits p bits in

  bitmatch bits with
  | { i' : 8;
      _ : n-8 : bitstring;
      j' : 8 : save_offset_to (j_offset);
      _ : m-n-8 : bitstring;
      k' : 8 : save_offset_to (k_offset) }
      when i = i' && j = j' && k = k' && j_offset = n && k_offset = m ->
      () (* ok *)
  | { _ } ->
      failwith (sprintf
		  "65_save_offset_to: test_bits: failed %d %d %d %d %d %d"
		  p i n j m k)

let () =
  for p = 0 to 4 do
    for n = 8 to 64 do
      for m = n+8 to 128 do
	List.iter (fun (i,j,k) -> test_bits (make_bits p i n j m k) p i n j m k)
	  [0x55, 0xaa, 0x33; 0x33, 0xaa, 0x55; 0x12, 0x34, 0x56]
      done;
    done;
  done
