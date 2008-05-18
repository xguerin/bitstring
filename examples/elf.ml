(* Read an ELF (Linux binary) header.
 * $Id$
 *)

open Printf

let () =
  let filename = "/bin/ls" in
  let bits = Bitmatch.bitstring_of_file filename in

  bitmatch bits with
  | { 0x7f : 8; "ELF" : 24 : string;	(* ELF magic number *)
      _ : 12*8 : bitstring;		(* ELF identifier *)
      e_type : 16 : littleendian;	(* object file type *)
      e_machine : 16 : littleendian	(* architecture *)
    } ->
      printf "%s: ELF binary, type %d, arch %d\n" filename e_type e_machine

  | { _ } ->
      eprintf "%s: Not an ELF binary\n" filename
