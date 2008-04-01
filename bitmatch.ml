(* Bitmatch library.
 * $Id: bitmatch.ml,v 1.3 2008-04-01 10:06:12 rjones Exp $
 *)

open Printf

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

(*----------------------------------------------------------------------*)
(* Extraction functions.
 *
 * NB: internal functions, called from the generated macros, and
 * the parameters should have been checked for sanity already).
 *)

(* Bitstrings. *)
let extract_bitstring data off len flen =
  (data, off, flen), off+flen, len-flen

let extract_remainder data off len =
  (data, off, len), off+len, 0

(* Extract and convert to numeric.  A single bit is returned as
 * a boolean.  There are no endianness or signedness considerations.
 *)
let extract_bit data off len _ =	(* final param is always 1 *)
  let byteoff = off lsr 3 in
  let bitmask = 1 lsl (7 - (off land 7)) in
  let b = Char.code data.[byteoff] land bitmask <> 0 in
  b, off+1, len-1

(* Extract [2..8] bits.  Because the result fits into a single
 * byte we don't have to worry about endianness, only signedness.
 *)
let extract_char_unsigned data off len flen =
  let byteoff = off lsr 3 in

  (* Extract the 16 bits at byteoff and byteoff+1 (note that the
   * second byte might not exist in the original string).
   *)
  let word =
    (Char.code data.[byteoff] lsl 8) +
      (if String.length data > byteoff+1 then Char.code data.[byteoff+1]
       else 0) in

  (* Mask off the top bits. *)
  let bitmask = (1 lsl (16 - (off land 7))) - 1 in
  let word = word land bitmask in
  (* Shift right to get rid of the bottom bits. *)
  let shift = 16 - ((off land 7) + flen) in
  let word = word lsr shift in

  word, off+flen, len-flen

(*----------------------------------------------------------------------*)
(* Display functions. *)

let isprint c =
  let c = Char.code c in
  c >= 32 && c < 127

let hexdump_bitstring chan (data, off, len) =
  let count = ref 0 in
  let off = ref off in
  let len = ref len in
  let linelen = ref 0 in
  let linechars = String.make 16 ' ' in

  fprintf chan "00000000  ";

  while !len > 0 do
    let bits = min !len 8 in
    let byte, off', len' = extract_char_unsigned data !off !len bits in
    off := off'; len := len';

    let byte = byte lsl (8-bits) in
    fprintf chan "%02x " byte;

    incr count;
    linechars.[!linelen] <-
      (let c = Char.chr byte in
       if isprint c then c else '.');
    incr linelen;
    if !linelen = 8 then fprintf chan " ";
    if !linelen = 16 then (
      fprintf chan " |%s|\n%08x  " linechars !count;
      linelen := 0;
      for i = 0 to 15 do linechars.[i] <- ' ' done
    )
  done;

  if !linelen > 0 then (
    let skip = (16 - !linelen) * 3 + if !linelen < 8 then 1 else 0 in
    for i = 0 to skip-1 do fprintf chan " " done;
    fprintf chan " |%s|\n" linechars
  ) else
    fprintf chan "\n"
