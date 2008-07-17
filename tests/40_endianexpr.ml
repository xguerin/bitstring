(* Endianness expressions
 * $Id$
 *)

open Printf
open Bitstring

let () =
  let rec loop = function
    | (e, expected) :: rest ->
	let bits = BITSTRING {
	  expected : 32 : endian (e);
	  expected : 32 : endian (e);
	  expected : 32 : endian (e)
	} in
	(bitmatch bits with
	 | { actual : 32 : endian (e);
	     actual : 32 : endian (e);
	     actual : 32 : endian (e) } ->
	     if actual <> expected then
	       failwith (sprintf "actual %ld <> expected %ld" actual expected)
	 | { _ } as bits ->
	     hexdump_bitstring stderr bits; exit 1
	);
	loop rest
    | [] -> ()
  in
  loop [
    BigEndian, 0xa1b2c3d4_l;
    BigEndian, 0xa1d4c3b2_l;
    LittleEndian, 0xa1b2c3d4_l;
    LittleEndian, 0xa1d4c3b2_l;
    NativeEndian, 0xa1b2c3d4_l;
    NativeEndian, 0xa1d4c3b2_l;
  ]
