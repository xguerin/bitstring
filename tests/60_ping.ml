(* Read in IPv4 and IPv6 ping packets and display them.
 * $Id: 60_ping.ml,v 1.2 2008-04-01 15:22:46 rjones Exp $
 *)

open Printf

let display pkt =
  bitmatch pkt with
  (* IPv4 packet header *)
  | 4 : 4; hdrlen : 4; tos : 8; length : 16;
    identification : 16; flags : 3; fragoffset : 13;
    ttl : 8; protocol : 8; checksum : 16;
    source : 32;
    dest : 32;
    options : (hdrlen-5)*32 : bitstring;
    payload : -1 : bitstring ->

    printf "IPv4:\n";
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

  (* IPv6 packet header *)
  | 6 : 4; tclass : 8; flow : 20;
    length : 16; nexthdr : 8; ttl : 8;
    source : 128 : bitstring;
    dest : 128 : bitstring;
    payload : -1 : bitstring ->

    printf "IPv6:\n";
    printf "  traffic class: %d\n" tclass;
    printf "  flow label: %d\n" flow;
    printf "  packet (payload) length: %d bytes\n" length;
    printf "  next header: %d\n" nexthdr;
    printf "  ttl: %d\n" ttl;
    printf "  source address:\n";
    Bitmatch.hexdump_bitstring stdout source;
    printf "  destination address:\n";
    Bitmatch.hexdump_bitstring stdout dest;
    printf "packet payload:\n";
    Bitmatch.hexdump_bitstring stdout payload

  | version : 4 ->
    eprintf "unknown IP version %d\n" version;
    exit 1

  | _ as pkt ->
    eprintf "data is smaller than one nibble:\n";
    Bitmatch.hexdump_bitstring stderr pkt;
    exit 1

let () =
  let pkt = Bitmatch.bitstring_of_file "tests/ipv4.ping" in
  display pkt;
  let pkt = Bitmatch.bitstring_of_file "tests/ipv6.ping" in
  display pkt
