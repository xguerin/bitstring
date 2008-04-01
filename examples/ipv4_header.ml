(* Parse and display an IPv4 header from a file.
 * $Id: ipv4_header.ml,v 1.2 2008-04-01 17:31:12 rjones Exp $
 *)

open Printf

let header = Bitmatch.bitstring_of_file "ipv4_header.dat"

let () =
  bitmatch header with
  | version : 4; hdrlen : 4; tos : 8; length : 16;
    identification : 16; flags : 3; fragoffset : 13;
    ttl : 8; protocol : 8; checksum : 16;
    source : 32;
    dest : 32;
    options : (hdrlen-5)*32 : bitstring;
    payload : -1 : bitstring
      when version = 4 ->

    printf "IPv%d:\n" version;
    printf "  header length: %d * 32 bit words\n" hdrlen;
    printf "  type of service: %d\n" tos;
    printf "  packet length: %d bytes\n" length;
    printf "  identification: %d\n" identification;
    printf "  flags: %d\n" flags;
    printf "  fragment offset: %d\n" fragoffset;
    printf "  ttl: %d\n" ttl;
    printf "  protocol: %d\n" protocol;
    printf "  checksum: %d\n" checksum;
    printf "  source: %lx  dest: %lx\n" source dest;
    printf "  header options + padding:\n";
    Bitmatch.hexdump_bitstring stdout options;
    printf "  packet payload:\n";
    Bitmatch.hexdump_bitstring stdout payload

  | version : 4 ->
    eprintf "cannot parse IP version %d\n" version

  | _ as header ->
    eprintf "data is smaller than one nibble:\n";
    Bitmatch.hexdump_bitstring stderr header
