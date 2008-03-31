(* Bitmatch library.
 * $Id: bitmatch.ml,v 1.1 2008-03-31 22:52:17 rjones Exp $
 *)

(* A bitstring is simply the data itself (as a string), and the
 * bitoffset and the bitlength within the string.  Note offset/length
 * are counted in bits, not bytes.
 *)
type bitstring = string * int * int

(* Functions to create and load bitstrings. *)
let empty_bitstring = "", 0, 0

let make_bitstring len c = String.make ((len+7) lsr 3) c, 0, len

let create_bitstring len = make_bitstring len '\000'

let bitstring_of_chan chan =
  let tmpsize = 16384 in
  let buf = Buffer.create tmpsize in
  let tmp = String.create tmpsize in
  let n = ref 0 in
  while n := input chan tmp 0 tmpsize; !n > 0 do
    Buffer.add_substring buf tmp 0 !n;
  done;
  Buffer.contents buf, 0, Buffer.length buf lsl 3

let bitstring_of_file fname =
  let chan = open_in_bin fname in
  let bs = bitstring_of_chan chan in
  close_in chan;
  bs

(* Extraction functions (internal: called from the generated macros,
 * and the parameters should have been checked for sanity already).
 *)
let extract_bitstring data off len flen =
  (data, off, flen), off+flen, len-flen

let extract_remainder data off len =
  (data, off, len), off+len, 0

(* Extract and convert to numeric. *)
let extract_bit data off len _ =	(* final param is always 1 *)
  let byteoff = off lsr 3 in
  let bitmask = 1 lsl (7 - (off land 7)) in
  let b = Char.code data.[byteoff] land bitmask <> 0 in
  b, off+1, len-1
