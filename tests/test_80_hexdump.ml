(* Test hexdump.
 * $Id$
 *)

open Printf

open Bitstring

let (//) = Filename.concat

let testdata = "tests" // "80_testdata" ;;
Sys.chdir testdata ;;

let diff = Bitstring_config.diff

let () =
  let files = Sys.readdir "." in
  let files = Array.to_list files in
  let files = List.filter (
    fun filename ->
      String.length filename > 3 &&
	filename.[0] = 'r' && filename.[1] = 'n' && filename.[2] = 'd'
  ) files in
  let files = List.map (
    fun filename ->
      let n = String.sub filename 3 (String.length filename - 3) in
      let n = int_of_string n in
      let bits = bitstring_of_file filename in
      (* 'bitstring_of_file' loads whole bytes.  Truncate it to
       * the real bit-length.
       *)
      let bits = takebits n bits in

      filename, n, bits
  ) files in

  (* Hexdump the bits, then compare using external 'diff' program. *)
  List.iter (
    fun (filename, n, bits) ->
      let output_filename = sprintf "hex%d.actual" n in
      let chan = open_out output_filename in
      hexdump_bitstring chan bits;
      close_out chan
  ) files;

  List.iter (
    fun (filename, n, bits) ->
      let actual_filename = sprintf "hex%d.actual" n in
      let expected_filename = sprintf "hex%d.expected" n in
      let cmd =
	sprintf "%s -u %s %s"
	  (Filename.quote diff)
	  (Filename.quote expected_filename)
	  (Filename.quote actual_filename) in
      if Sys.command cmd <> 0 then (
	exit 1
      )
  ) files
