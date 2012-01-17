(* Regression test for bug in concatenation found by Phil Tomson.
 * $Id$
 *)

open Printf
open Bitstring

let errors = ref 0

let () =
  let bs_256 = ones_bitstring 256 in
  assert (bitstring_length bs_256 = 256);

  let bs2 =
    BITSTRING {
      false : 1;
      (subbitstring bs_256 0 66) : 66 : bitstring
    } in
  let len = bitstring_length bs2 in
  if len <> 67 then (
    eprintf "invalid length of bs2: len = %d, expected 67\n" len;
    hexdump_bitstring stderr bs2;
    incr errors
  );

  let bs3 =
    BITSTRING {
      false : 1;
      (subbitstring bs_256 0 66) : 66 : bitstring;
      (subbitstring bs_256 66 67) : 67 : bitstring
    } in
  let len = bitstring_length bs3 in
  if len <> 134 then (
    eprintf "invalid length of bs3: len = %d, expected 134\n" len;
    hexdump_bitstring stderr bs3;
    incr errors
  );

  let bs4 =
    BITSTRING {
      (subbitstring bs_256 66 67) : 67 : bitstring
    } in
  let len = bitstring_length bs4 in
  if len <> 67 then (
    eprintf "invalid length of bs4: len = %d, expected 67\n" len;
    hexdump_bitstring stderr bs4;
    incr errors
  );

  let bs5 = concat [subbitstring bs_256 0 66; subbitstring bs_256 66 67] in
  let len = bitstring_length bs5 in
  if len <> 133 then (
    eprintf "invalid length of bs5: len = %d, expected 133\n" len;
    hexdump_bitstring stderr bs5;
    incr errors
  );

  let bs6 = concat [ subbitstring bs_256 0 64; subbitstring bs_256 64 64] in
  let len = bitstring_length bs6 in
  if len <> 128 then (
    eprintf "invalid length of bs6: len = %d, expected 128\n" len;
    hexdump_bitstring stderr bs6;
    incr errors
  );

  let bs7 = concat [ subbitstring bs_256 0 65; subbitstring bs_256 65 64] in
  let len = bitstring_length bs7 in
  if len <> 129 then (
    eprintf "invalid length of bs7: len = %d, expected 129\n" len;
    hexdump_bitstring stderr bs7;
    incr errors
  );

  if !errors <> 0 then exit 1
