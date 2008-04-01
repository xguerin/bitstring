(* Test the hexdump function.
 * $Id: 03_hexdump.ml,v 1.1 2008-04-01 10:06:12 rjones Exp $
 *)

open Printf

let bits = Bitmatch.make_bitstring (32*8) '\x5a'

let () =
  Bitmatch.hexdump_bitstring stdout bits;

  let data, off, len = bits in
  let bits = data, off+1, len-1 in
  Bitmatch.hexdump_bitstring stdout bits;

  let data, off, len = bits in
  let bits = data, off+1, len-1 in
  Bitmatch.hexdump_bitstring stdout bits
