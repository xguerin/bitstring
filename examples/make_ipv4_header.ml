(* Create an IPv4 header.
 * $Id: make_ipv4_header.ml,v 1.1 2008-03-31 22:52:17 rjones Exp $
 *)

open Printf

let version = 4
let hdrlen = 5				(* no options *)
let tos = 16
let length = 64				(* total packet length *)
let identification = 0
let flags = 0
let fragoffset = 0
let ttl = 255
let protocol = 17			(* UDP *)
let checksum = 0
let source = 0xc0a80202			(* 192.168.2.2 *)
let dest = 0xc0a80201			(* 192.168.2.1 *)
let options = Bitmatch.empty_bitstring
let payload_length = (length - hdrlen*4) * 8
let payload = Bitmatch.create_bitstring payload_length

let header =
  <| version : 4; hdrlen : 4; tos : 8; length : 16;
     identification : 16; flags : 3; fragoffset : 13;
     ttl : 8; protocol : 8; checksum : 16;
     source : 32;
     dest : 32;
     options : -1, bitstring;
     payload : payload_length, bitstring |>

(*
  generates:

  let header = Bitmatch.join_bitstrings [
    Bitmatch.create_unsigned_be version 4;
    Bitmatch.create_unsigned_be hdrlen 4; (* etc. *)
    options;
    Bitmatch.check_bitstring_length payload payload_length
  ]

  which can throw an exception if values are out of range.
*)

let () = Bitmatch.file_of_bitstring header "ipv4_header_out.dat"
