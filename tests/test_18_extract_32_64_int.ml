(* Test fix for a regression when extracting 32 and 64 bit aligned
 * integers (discovered / fixed / tested by Hans Ole Rafaelsen).
 * $Id$
 *)

open Printf

open Bitstring

let bitstring_of_int32 i =
  BITSTRING { i : 32 }

let bitstring_of_int64 i =
  BITSTRING { i : 64 }

let int32_of_bitstring bits =
  bitmatch bits with
  | { i : 32 } -> i

let int64_of_bitstring bits =
  bitmatch bits with
  | { i : 64 } -> i

let () =
  let b1 = bitstring_of_int32 1_l in
  let b2 = bitstring_of_int32 2_l in
  let b3 = bitstring_of_int32 3_l in
  let i1 = int32_of_bitstring b1 in
  let i2 = int32_of_bitstring b2 in
  let i3 = int32_of_bitstring b3 in
  assert (i1 = 1_l);
  assert (i2 = 2_l);
  assert (i3 = 3_l);

  let b1 = bitstring_of_int64 1_L in
  let b2 = bitstring_of_int64 2_L in
  let b3 = bitstring_of_int64 3_L in
  let i1 = int64_of_bitstring b1 in
  let i2 = int64_of_bitstring b2 in
  let i3 = int64_of_bitstring b3 in
  assert (i1 = 1_L);
  assert (i2 = 2_L);
  assert (i3 = 3_L)
