(* Compare bitstrings.
 * $Id$
 *)

open Printf

let sgn = function
  | 0 -> 0
  | i when i > 0 -> 1
  | _ -> -1

let () =
  for i = 0 to 33 do
    for j = 0 to 33 do
      let bits1 = Bitstring.ones_bitstring i
      and bits2 = Bitstring.ones_bitstring j in
      let r = Bitstring.compare bits1 bits2 in
      if sgn r <> sgn (compare i j) then (
	eprintf "ones compare failed %d %d %d\n" i j r;
	exit 1
      )
    done
 done;
 for i = 0 to 33 do
   for j = 0 to 33 do
     let bits1 = Bitstring.zeroes_bitstring i
     and bits2 = Bitstring.zeroes_bitstring j in
     let r = Bitstring.compare bits1 bits2 in
     if sgn r <> sgn (compare i j) then (
       eprintf "zeroes compare failed %d %d %d\n" i j r;
       exit 1
     )
   done
 done;
 for i = 0 to 33 do
   for j = 0 to 33 do
     let bits1 = Bitstring.make_bitstring i '\x55'
     and bits2 = Bitstring.make_bitstring j '\x55' in
     let r = Bitstring.compare bits1 bits2 in
     if sgn r <> sgn (compare i j) then (
       eprintf "x55 compare failed %d %d %d\n" i j r;
       exit 1
     )
   done
 done;
 for i = 0 to 33 do
   for j = 0 to 33 do
     let bits1 = Bitstring.make_bitstring i '\x55' in
     let bits2 = Bitstring.make_bitstring i '\x55' in
     let bits2 = Bitstring.concat [Bitstring.zeroes_bitstring j; bits2] in
     assert (Bitstring.bitstring_length bits2 = j+i);
     let bits2 = Bitstring.dropbits j bits2 in
     assert (Bitstring.bitstring_length bits2 = i);
     let r = Bitstring.compare bits1 bits2 in
     if r <> 0 then (
       eprintf "x55 non-aligned compare failed %d %d %d\n" i j r;
       exit 1
     )
   done
 done
