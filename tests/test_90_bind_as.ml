(* Regression test for bug in 'as-binding' found by Matej Kosik.
 * $Id$
 *)

open Printf
open Bitstring

let bits = Bitstring.ones_bitstring 1 in
bitmatch bits with
| { _ : 1 } as foo ->
  let len = Bitstring.bitstring_length foo in
  if len <> 1 then (
    hexdump_bitstring stderr foo;
    eprintf "test error: length = %d, expecting 1\n" len;
    exit 1
  )
| { _ } ->
  assert false
