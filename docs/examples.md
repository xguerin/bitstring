# Examples

## IPv4 packets

```ocaml
match%bitstring pkt with
(* IPv4 packet header
  0                   1                   2                   3   
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |   4   |  IHL  |Type of Service|          Total Length         |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |         Identification        |Flags|      Fragment Offset    |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |  Time to Live |    Protocol   |         Header Checksum       |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |                       Source Address                          |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |                    Destination Address                        |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |                    Options                    |    Padding    |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
*)
| {| 4 : 4; hdrlen : 4; tos : 8;   length : 16;
     identification : 16;          flags : 3; fragoffset : 13;
     ttl : 8; protocol : 8;        checksum : 16;
     source : 32;
     dest : 32;
     options : (hdrlen-5)*32 : bitstring;
     payload : -1 : bitstring
   |} ->

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
  Bitstring.hexdump_bitstring stdout options;
  printf "  packet payload:\n";
  Bitstring.hexdump_bitstring stdout payload

| {| version : 4 |} ->
  eprintf "unknown IP version %d\n" version;
  exit 1

| {| _ |} as pkt ->
  eprintf "data is smaller than one nibble:\n";
  Bitstring.hexdump_bitstring stderr pkt;
  exit 1
```

## EXT3 superblock parser

```ocaml
let bits = Bitstring.bitstring_of_file "tests/ext3_sb"

let () =
  match%bitstring bits with
  | {| s_inodes_count : 32 : littleendian;       (* Inodes count *)
       s_blocks_count : 32 : littleendian;       (* Blocks count *)
       s_r_blocks_count : 32 : littleendian;     (* Reserved blocks count *)
       s_free_blocks_count : 32 : littleendian;  (* Free blocks count *)
       s_free_inodes_count : 32 : littleendian;  (* Free inodes count *)
       s_first_data_block : 32 : littleendian;   (* First Data Block *)
       s_log_block_size : 32 : littleendian;     (* Block size *)
       s_log_frag_size : 32 : littleendian;      (* Fragment size *)
       s_blocks_per_group : 32 : littleendian;   (* # Blocks per group *)
       s_frags_per_group : 32 : littleendian;    (* # Fragments per group *)
       s_inodes_per_group : 32 : littleendian;   (* # Inodes per group *)
       s_mtime : 32 : littleendian;              (* Mount time *)
       s_wtime : 32 : littleendian;              (* Write time *)
       s_mnt_count : 16 : littleendian;          (* Mount count *)
       s_max_mnt_count : 16 : littleendian;      (* Maximal mount count *)
       0xef53 : 16 : littleendian |} ->          (* Magic signature *)

    printf "ext3 superblock:\n";
    printf "  s_inodes_count = %ld\n" s_inodes_count;
    printf "  s_blocks_count = %ld\n" s_blocks_count;
    printf "  s_free_inodes_count = %ld\n" s_free_inodes_count;
    printf "  s_free_blocks_count = %ld\n" s_free_blocks_count

  | {| _ |} ->
    eprintf "not an ext3 superblock!\n%!";
    exit 2
```

## Simple binary message parser

```ocaml
(* +---------------+---------------+--------------------------+
   | type          | subtype       | parameter                |
   +---------------+---------------+--------------------------+
    <-- 16 bits --> <-- 16 bits --> <------- 32 bits -------->

   All fields are in network byte order. *)

let%bitstring make_message typ subtype param = {|
  typ : 16;
  subtype : 16;
  param : 32
|};;
```


