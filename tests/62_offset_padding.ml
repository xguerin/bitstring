(* Test computed offsets when original_off <> 0.
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
      j' : 8 : offset(n);
      k' : 8 : offset(m) } when i = i' && j = j' && k = k' -> () (* ok *)
  | { _ } ->
      failwith (sprintf "60_simple_offset: test_bits: failed %d %d %d %d %d"
		  i n j m k)

let () =
  for p = 1 to 4 do
    for n = 8 to 128 do
      for m = n+8 to 256 do
	List.iter (fun (i,j,k) -> test_bits (make_bits p i n j m k) p i n j m k)
	  [0x55, 0xaa, 0x33; 0x33, 0xaa, 0x55; 0x12, 0x34, 0x56]
      done;
    done;
  done
