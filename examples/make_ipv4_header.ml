(* Create an IPv4 header.
 * $Id$
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
let source = 0xc0a80202_l		(* 192.168.2.2 *)
let dest = 0xc0a80201_l			(* 192.168.2.1 *)
let options = Bitstring.empty_bitstring
let payload_length = (length - hdrlen*4) * 8
let payload = Bitstring.create_bitstring payload_length

let%bitstring header =
  {|
    version : 4; hdrlen : 4; tos : 8; length : 16;
    identification : 16; flags : 3; fragoffset : 13;
    ttl : 8; protocol : 8; checksum : 16;
    source : 32; dest : 32
  |}

let () = Bitstring.bitstring_to_file header "ipv4_header_out.dat"
