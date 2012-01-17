(* Test the various functions to load bitstrings from files.
 * $Id$
 *)

open Printf
open Bitstring

let () =
  let bits1 =
    let b1 = make_bitstring 800 '\x5a' in
    let b2 = make_bitstring 400 '\x88' in (
      BITSTRING {
	b1 : 800 : bitstring;
	b2 : 400 : bitstring
      }
    ) in
  let bits2 = (
    let b = make_bitstring 800 '\xaa' in
    BITSTRING {
      b : 800 : bitstring
    }
  ) in
  let bits = concat [bits1; bits2] in
  let filename, chan =
    Filename.open_temp_file ~mode:[Open_binary] "bitstring_test" ".tmp" in
  bitstring_to_chan bits chan;
  close_out chan;

  let bits' = bitstring_of_file filename in
  assert (equals bits bits');

  let chan = open_in filename in
  let bits' = bitstring_of_chan chan in
  close_in chan;
  assert (equals bits bits');

  let chan = open_in filename in
  let bits' = bitstring_of_chan_max chan 150 in
  assert (equals bits1 bits');
  let bits' = bitstring_of_chan_max chan 100 in
  assert (equals bits2 bits');
  close_in chan;

  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  let bits' = bitstring_of_file_descr fd in
  Unix.close fd;
  assert (equals bits bits');

  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  let bits' = bitstring_of_file_descr_max fd 150 in
  assert (equals bits1 bits');
  let bits' = bitstring_of_file_descr_max fd 100 in
  assert (equals bits2 bits');
  Unix.close fd;

  Unix.unlink filename
