(* Print out packets from a tcpdump / libpcap / wireshark capture file.
 * $Id$
 *
 * To test this, capture some data using:
 *   /usr/sbin/tcpdump -s 1500 -w /tmp/dump
 * then analyze it using:
 *   ./libpcap /tmp/dump
 *
 * The file format is documented here:
 *   http://wiki.wireshark.org/Development/LibpcapFileFormat
 *
 * libpcap endianness is determined at runtime.  Currently we don't
 * handle this well - we need to write the match code out twice.
 * Runtime endianness setting will solve this.  See TODO list, item 11.
 *)

open Printf

let rec main () =
  if Array.length Sys.argv <= 1 then failwith "libpcap dumpfile";
  let bits = Bitmatch.bitstring_of_file Sys.argv.(1) in
  let endian, file_header, bits = libpcap_header bits in

  (* Read the packets and print them out. *)
  let rec loop bits =
    let pkt_header, pkt_data, bits = libpcap_packet endian file_header bits in
    decode_and_print_packet file_header pkt_header pkt_data;
    loop bits
  in
  try loop bits
  with
    End_of_file -> ()

and libpcap_header bits =
  bitmatch bits with
  | { 0xd4c3b2a1_l : 32;		(* writer was little endian *)
      major : 16 : littleendian;	(* version *)
      minor : 16 : littleendian;
      timezone : 32 : littleendian;	(* timezone correction (seconds) *)
      _ : 32 : littleendian;		(* always 0 apparently *)
      snaplen : 32 : littleendian;	(* max length of captured packets *)
      network : 32 : littleendian;	(* data link layer type *)
      rest : -1 : bitstring
    } ->
      Bitmatch.LittleEndian, (major, minor, timezone, snaplen, network), rest

  | { 0xa1b2c3d4_l : 32;		(* writer was big endian *)
      major : 16;			(* version *)
      minor : 16;
      timezone : 32;		        (* timezone correction (seconds) *)
      _ : 32;				(* always 0 apparently *)
      snaplen : 32;		        (* max length of captured packets *)
      network : 32;			(* data link layer type *)
      rest : -1 : bitstring
    } ->
      Bitmatch.BigEndian, (major, minor, timezone, snaplen, network), rest

  | { _ } ->
      failwith "not a libpcap/tcpdump packet capture file"

and libpcap_packet endian file_header bits =
  if endian = Bitmatch.LittleEndian then (
    bitmatch bits with
    | { ts_sec : 32 : littleendian;	(* packet timestamp seconds *)
	ts_usec : 32 : littleendian;	(* packet timestamp microseconds *)
	incl_len : 32 : littleendian;   (* packet length saved in this file *)
	orig_len : 32 : littleendian;   (* packet length originally on wire *)
	pkt_data : Int32.to_int incl_len*8 : bitstring;
	rest : -1 : bitstring
      } ->
	(ts_sec, ts_usec, incl_len, orig_len), pkt_data, rest

    | { _ } -> raise End_of_file
  ) else (
    bitmatch bits with
    | { ts_sec : 32;			(* packet timestamp seconds *)
	ts_usec : 32;		        (* packet timestamp microseconds *)
	incl_len : 32;		        (* packet length saved in this file *)
	orig_len : 32;		        (* packet length originally on wire *)
	pkt_data : Int32.to_int incl_len*8 : bitstring;
	rest : -1 : bitstring
      } ->
	(ts_sec, ts_usec, incl_len, orig_len), pkt_data, rest

    | { _ } -> raise End_of_file
  )

and decode_and_print_packet file_header pkt_header pkt_data =
  let (ts_sec, ts_usec, _, orig_len) = pkt_header in
  printf "%ld.%ld %ldB " ts_sec ts_usec orig_len;

  (* Assume an ethernet frame containing an IPv4/6 packet.  We ignore
   * the ethertype field and determine the IP version from the packet
   * itself.  If it doesn't match our assumptions, hexdump it.
   *)
  (bitmatch pkt_data with
   | { d0 : 8; d1 : 8; d2 : 8; d3 : 8; d4 : 8; d5 : 8; (* ether dest *)
       s0 : 8; s1 : 8; s2 : 8; s3 : 8; s4 : 8; s5 : 8; (* ether src *)
       _ : 16;					      (* ethertype *)
       packet : -1 : bitstring			      (* payload *)
     } ->
       printf "%x:%x:%x:%x:%x:%x < %x:%x:%x:%x:%x:%x "
	 d0 d1 d2 d3 d4 d5 s0 s1 s2 s3 s4 s5;

       (bitmatch packet with
	| { 4 : 4;			(* IPv4 *)
	    hdrlen : 4; tos : 8; length : 16;
	    identification : 16; flags : 3; fragoffset : 13;
	    ttl : 8; protocol : 8; checksum : 16;
	    s0 : 8; s1 : 8; s2 : 8; s3 : 8;
	    d0 : 8; d1 : 8; d2 : 8; d3 : 8;
	    _(*options*) : (hdrlen-5)*32 : bitstring;
	    _(*payload*) : -1 : bitstring } ->
	    printf "IPv4 %d.%d.%d.%d < %d.%d.%d.%d "
	      s0 s1 s2 s3 d0 d1 d2 d3

	| { 6 : 4;			(* IPv6 *)
	    tclass : 8; flow : 20;
	    length : 16; nexthdr : 8; ttl : 8;
	    _(*source*) : 128 : bitstring;
	    _(*dest*) : 128 : bitstring;
	    _(*payload*) : -1 : bitstring } ->
	    printf "IPv6 ";

	| { _ } ->
	    printf "\n"; Bitmatch.hexdump_bitstring stdout packet
       )

   | { _ } ->
       printf "\n"; Bitmatch.hexdump_bitstring stdout pkt_data
  );
  printf "\n"

let () = main ()
