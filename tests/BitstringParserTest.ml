(* Copyright (c) 2016 Xavier R. Guérin <xguerin@users.noreply.github.com>
  
   Permission to use, copy, modify, and distribute this software for any purpose
   with or without fee is hereby granted, provided that the above copyright
   notice and this permission notice appear in all copies.
  
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
   OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

open OUnit2
open Bitstring

(* EXT3 superblock parsing test *)

let ext3_test context =
  let bits = Bitstring.bitstring_of_file "../../../tests/data/ext3_sb" in
  match%bitstring bits with
  (* Check if the file is an EXT3 superblock *)
  | {|  50200_l   : 32 : littleendian  (* Inodes count *)
     ;  _         : 32 : littleendian  (* Blocks count *)
     ;  _         : 32 : littleendian  (* Reserved blocks count *)
     ;  155333_l  : 32 : littleendian  (* Free blocks count *)
     ;  50150_l   : 32 : littleendian  (* Free inodes count *)
     ;  _         : 32 : littleendian  (* First Data Block *)
     ;  _         : 32 : littleendian  (* Block size *)
     ;  _         : 32 : littleendian  (* Fragment size *)
     ;  _         : 32 : littleendian  (* # Blocks per group *)
     ;  _         : 32 : littleendian  (* # Fragments per group *)
     ;  _         : 32 : littleendian  (* # Inodes per group *)
     ;  _         : 32 : littleendian  (* Mount time *)
     ;  _         : 32 : littleendian  (* Write time *)
     ;  _         : 16 : littleendian  (* Mount count *)
     ;  _         : 16 : littleendian  (* Maximal mount count *)
     ;  0xef53    : 16 : littleendian  (* Magic signature *)
    |}
    -> ()
  (* Otherwise, throw an error *)
  | {| _ |} -> failwith "Invalid EXT3 superblock"
;;

(* GIF parser test *)

let gif_test context =
  let bits = Bitstring.bitstring_of_file "../../../tests/data/sad_face.gif" in
  match%bitstring bits with
  (* Check if the file is a GIF image *)
  | {|  ("GIF87a" | "GIF89a")  : 6*8 : string        ; (* GIF magic. *)
        2145                   : 16  : littleendian  ;
        2145                   : 16  : littleendian  ;
        true                   : 1                   ; (* Has colormap? *)
        7                      : 3                   ; (* Color res = colorbits+1 *)
        false                  : 1                   ;
        7                      : 3                   ; (* Bits/pixel = bps+1 *)
        0                      : 8                   ; (* Background colo *)
        0                      : 8
    |}
    -> ()
  (* Otherwise, throw an error *)
  | {| _ |} -> failwith "Invalid GIF image"
;;

(* PCAP parser test *)

let to_bitstring_endian = function
  | 0xa1b2c3d4_l | 0xa1b23c4d_l -> Bitstring.BigEndian
  | 0xd4c3b2a1_l | 0x4d3cb2a1_l -> Bitstring.LittleEndian
  | _ -> failwith "Unknown PCAP format"
;;

let pcap_ipv4_test context ipv4 =
  match%bitstring ipv4 with
  | {|  4       : 4;
        5       : 4;
        0       : 8;                (* dscn/ecn *)
        60      : 16  : bigendian;
        0x92A6  : 16  : bigendian;  (* ident *)
        0x02    : 3;                (* flags *)
        0       : 13  : bigendian;  (* fragment offset *)
        64      : 8;                (* ttl *)
        0x06    : 8;
        0xFA91  : 16  : bigendian;  (* checksum *)
        0xC0    : 8;
        0xA8    : 8;
        0x01    : 8;
        0x21    : 8;                (* source IP *)
        0xCC    : 8;
        0xB2    : 8;
        0x1F    : 8;
        0x08    : 8;                (* destination IP *)
        _       : -1  : bitstring
    |}
    -> ()
  | {| _ |} -> failwith "Not a valid IPv4 layer"
;;

let pcap_eth_test context eth =
  match%bitstring eth with
  | {|  0x00    : 8;
        0xA0    : 8;
        0xC5    : 8;
        0x8F    : 8;
        0xE3    : 8;
        0xC7    : 8;                (* destination MAC *)
        0x00    : 8;
        0x0C    : 8;
        0x76    : 8;
        0x1C    : 8;
        0x1B    : 8;
        0x97    : 8;                (* source MAC *)
        0x0800  : 16  : bigendian;  (* EtherType *)
        ipv4    : -1  : bitstring
    |}
    -> pcap_ipv4_test context ipv4
  | {| _ |} -> failwith "Not a valid Ethernet layer"
;;

let pcap_packet_test context endian packet =
  match%bitstring packet with
  | {|  _         : 32;
        _         : 32;
        incl_len  : 32                          : endian (endian);
        orig_len  : 32                          : endian (endian);
        eth       : (Int32.to_int incl_len) * 8 : bitstring
    |}
    -> pcap_eth_test context eth
  | {| _ |} -> failwith "Not a valid packet descriptor"
;;

let pcap_test context =
  let bits = Bitstring.bitstring_of_file "../../../tests/data/net.pcap" in
  match%bitstring bits with
  (* Check if the file is a PCAP file *)
  | {|  ((0xa1b2c3d4_l |
          0xa1b23c4d_l |
          0xd4c3b2a1_l |
          0x4d3cb2a1_l) as magic) : 32;
        2                         : 16 : littleendian;  (* major *)
        4                         : 16 : littleendian;  (* minor *)
        _                         : 32;                 (* time zone *)
        0_l                       : 32;                 (* unused *)
        _                         : 32;                 (* snaplen *)
        _                         : 32;                 (* network *)
        packet                    : -1 : bitstring
    |}
    -> pcap_packet_test context (to_bitstring_endian magic) packet
  (* Otherwise, throw an error *)
  | {| _ |} -> failwith "Not a valid PCAP file"
;;

(* Function-style parser test *)

let function_parser = function%bitstring
  | {|  1       : 3
      ; 2       : 4
      ; "hello" : 40 : string
    |}
    -> assert_bool "Bitstring is valid" true
  | {| _ |} -> assert_bool "Invalid bitstring" false
;;

let function_parser_test context =
  [%bitstring {| 1 : 3; 2 : 4; "hello" : 40 : string |}] |> function_parser
;;

(* Function-style parser test, inline *)

let function_parser_inline_test context =
  [%bitstring {| 1 : 3; 2 : 4; "hello" : 40 : string |}]
  |> function%bitstring
  | {|  1       : 3
      ; 2       : 4
      ; "hello" : 40 : string
    |}
    -> assert_bool "Bitstring is valid" true
  | {| _ |} -> assert_bool "Invalid bitstring" false
;;

(* Parser with a guard (PR#16) *)

let parser_with_guard_test context =
  let bits = Bitstring.bitstring_of_string "abc" in
  match%bitstring bits with
  | {| "abc" : 24 : string |} when false -> assert_bool "Guard was ignored" false
  | {| _ |} -> assert_bool "Guard was honored" true
;;

(* Wrong fastpath extraction function #46 *)

let wrong_fp_extraction context =
  let mb = Bytes.of_string "\000\000\145", 0, 24 in
  match%bitstring mb with
  | {| matched_value : 24 : bigendian |} -> assert_equal matched_value 145
  | {| _ |} -> assert_bool "Invalid bitstring" false
;;

let wrong_fp_extraction_dynamic context =
  let mb = Bytes.of_string "\000\000\000\145", 0, 32
  and on = 8 in
  match%bitstring mb with
  | {| _ : on ; matched_value : 24 : bigendian |} -> assert_equal matched_value 145
  | {| _ |} -> assert_bool "Invalid bitstring" false
;;

(* Wrong LE extraction on partial int64. *)

let wrong_le_partial_int64_extraction context =
  (* Forward. *)
  let mb = Bytes.of_string "\xA0\x00\x00\x00\x00\x00\x00\x00", 0, 64 in
  match%bitstring mb with
  | {| a:4; b:60:littleendian |} ->
    assert_equal a 10;
    assert_equal b 0L
  | {| _ |} ->
    assert_bool "Invalid bitstring" false;
    (* Backward. *)
    let mb = Bytes.of_string "\x00\x00\x00\x00\x00\x00\x00\x0A", 0, 64 in
    (match%bitstring mb with
     | {| b:60:littleendian; a:4 |} ->
       assert_equal a 10;
       assert_equal b 0L
     | {| _ |} -> assert_bool "Invalid bitstring" false)
;;

(* Test suite definition *)

let suite =
  "BitstringParserTest"
  >::: [ "ext3" >:: ext3_test
       ; "gif" >:: gif_test
       ; "pcap" >:: pcap_test
       ; "function" >:: function_parser_test
       ; "function_inline" >:: function_parser_inline_test
       ; "parser_with_guard" >:: parser_with_guard_test
       ; "wrong_fp_extraction" >:: wrong_fp_extraction
       ; "wrong_fp_extraction_dynamic" >:: wrong_fp_extraction_dynamic
       ; "wrong_le_partial_int64_extraction" >:: wrong_le_partial_int64_extraction
       ]
;;
