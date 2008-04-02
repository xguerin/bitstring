(** Bitmatch library. *)
(* Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * $Id: bitmatch.mli,v 1.9 2008-04-02 11:06:31 rjones Exp $
 *)

(**
   {2 Introduction}

   Bitmatch adds Erlang-style bitstrings and matching over bitstrings
   as a syntax extension and library for OCaml.  You can use
   this module to both parse and generate binary formats.

   {2 Examples}

   A function which can parse IPv4 packets:

{[
let display pkt =
  bitmatch pkt with
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
  | 4 : 4; hdrlen : 4; tos : 8;   length : 16;
    identification : 16;          flags : 3; fragoffset : 13;
    ttl : 8; protocol : 8;        checksum : 16;
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

  | version : 4 ->
    eprintf "unknown IP version %d\n" version;
    exit 1

  | _ as pkt ->
    eprintf "data is smaller than one nibble:\n";
    Bitmatch.hexdump_bitstring stderr pkt;
    exit 1
]}

   A program which can parse
   {{:http://lxr.linux.no/linux/include/linux/ext3_fs.h}Linux EXT3 filesystem superblocks}:

{[
let bits = Bitmatch.bitstring_of_file "tests/ext3_sb"

let () =
  bitmatch bits with
  | s_inodes_count : 32 : littleendian;       (* Inodes count *)
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
    0xef53 : 16 : littleendian ->             (* Magic signature *)

    printf "ext3 superblock:\n";
    printf "  s_inodes_count = %ld\n" s_inodes_count;
    printf "  s_blocks_count = %ld\n" s_blocks_count;
    printf "  s_free_inodes_count = %ld\n" s_free_inodes_count;
    printf "  s_free_blocks_count = %ld\n" s_free_blocks_count

  | _ ->
    eprintf "not an ext3 superblock!\n%!";
    exit 2
]}

   Constructing packets for a simple binary message
   protocol:

{[
(*
  +---------------+---------------+--------------------------+
  | type          | subtype       | parameter                |
  +---------------+---------------+--------------------------+
   <-- 16 bits --> <-- 16 bits --> <------- 32 bits -------->

  All fields are in network byte order.
*)

let make_message typ subtype param =
  (BITSTRING
     typ : 16;
     subtype : 16;
     param : 32) ;;
]}

   {2 Loading, creating bitstrings}

   The basic data type is the {!bitstring}, a string of bits of
   arbitrary length.  Bitstrings can be any length in bits and
   operations do not need to be byte-aligned (although they will
   generally be more efficient if they are byte-aligned).

   Internally a bitstring is stored as a normal OCaml [string]
   together with an offset and length, where the offset and length are
   measured in bits.  Thus one can efficiently form substrings of
   bitstrings, overlay a bitstring on existing data, and load and save
   bitstrings from files or other external sources.

   To load a bitstring from a file use {!bitstring_of_file} or
   {!bitstring_of_chan}.

   There are also functions to create bitstrings from arbitrary data.
   See the reference below.

   {2 Matching bitstrings with patterns}

   Use the [bitmatch] operator (part of the syntax extension) to break
   apart a bitstring into its fields.  [bitmatch] works a lot like the
   OCaml [match] operator.

   The general form of [bitmatch] is:

   [bitmatch] {i bitstring-expression} [with]

   [|] {i pattern} [->] {i code}

   [|] {i pattern} [->] {i code}

   [|] ...

   As with normal match, the statement attempts to match the
   bitstring against each pattern in turn.  If none of the patterns
   match then the standard library [Match_failure] exception is
   thrown.

   Patterns look a bit different from normal match patterns.  The
   consist of a list of bitfields separated by [;] where each bitfield
   contains a bind variable, the width (in bits) of the field, and
   other information.  Some example patterns:

{[
bitmatch bits with

| version : 8; name : 8; param : 8 -> ...

   (* Bitstring of at least 3 bytes.  First byte is the version
      number, second byte is a field called name, third byte is
      a field called parameter. *)

| flag : 1 ->
   printf "flag is %b\n" flag

   (* A single flag bit (mapped into an OCaml boolean). *)

| len : 4; data : 1+len ->
   printf "len = %d, data = 0x%Lx\n" len data

   (* A 4-bit length, followed by 1-16 bits of data, where the
      length of the data is computed from len. *)

| ipv6_source : 128 : bitstring;
  ipv6_dest : 128 : bitstring -> ...

   (* IPv6 source and destination addresses.  Each is 128 bits
      and is mapped into a bitstring type which will be a substring
      of the main bitstring expression. *)
]}

   You can also add conditional when-clauses:

{[
| version : 4
    when version = 4 || version = 6 -> ...

   (* Only match and run the code when version is 4 or 6.  If
      it isn't we will drop through to the next case. *)
]}

   Note that the pattern is only compared against the first part of
   the bitstring (there may be more data in the bitstring following
   the pattern, which is not matched).  In terms of regular
   expressions you might say that the pattern matches [^pattern], not
   [^pattern$].  To ensure that the bitstring contains only the
   pattern, add a length -1 bitstring to the end and test that its
   length is zero in the when-clause:

{[
| n : 4;
  rest : -1 : bitstring
    when Bitmatch.bitstring_length rest = 0 -> ...

   (* Only matches exactly 4 bits. *)
]}

   Normally the first part of each field is a binding variable,
   but you can also match a constant, as in:

{[
| 6 : 4 -> ...

   (* Only matches if the first 4 bits contain the integer 6. *)
]}

   {3 Pattern field reference}

   The exact format of each pattern field is:

   [pattern : length [: qualifier [,qualifier ...]]]

   [pattern] is the pattern, binding variable name, or constant to
   match.  [length] is the length in bits which may be either a
   constant or an expression.  The length expression is just an OCaml
   expression and can use any values defined in the program, and refer
   back to earlier fields (but not to later fields).

   Integers can only have lengths in the range [1..64] bits.  See the
   {{:#integertypes}integer types} section below for how these are
   mapped to the OCaml int/int32/int64 types.  This is checked
   at compile time if the length expression is constant, otherwise it is
   checked at runtime and you will get a runtime exception eg. in
   the case of a computed length expression.

   A bitstring field of length -1 matches all the rest of the
   bitstring (thus this is only useful as the last field in a
   pattern).

   A bitstring field of length 0 matches an empty bitstring
   (occasionally useful when matching optional subfields).

   Qualifiers are a list of identifiers which control the type,
   signedness and endianness of the field.  Permissible qualifiers are:

   - [int] (field has an integer type)
   - [bitstring] (field is a bitstring type)
   - [signed] (field is signed)
   - [unsigned] (field is unsigned)
   - [bigendian] (field is big endian - a.k.a network byte order)
   - [littleendian] (field is little endian - a.k.a Intel byte order)
   - [nativeendian] (field is same endianness as the machine)

   The default settings are [int], [unsigned], [bigendian].

   Note that many of these qualifiers cannot be used together,
   eg. bitstrings do not have endianness.  The syntax extension should
   give you a compile-time error if you use incompatible qualifiers.

   {3 Other cases in bitmatch}

   As well as a list of fields, it is possible to name the
   bitstring and/or have a default match case:

{[
| _ -> ...

   (* Default match case. *)

| _ as pkt -> ...

   (* Default match case, with 'pkt' bound to the whole bitstring. *)
]}

   {2 Constructing bitstrings}

   Bitstrings may be constructed using the [BITSTRING] operator (as an
   expression).  The [BITSTRING] operator takes a list of fields,
   similar to the list of fields for matching:

{[
let version = 1 ;;
let data = 10 ;;
let bits =
  BITSTRING
    version : 4;
    data : 12 ;;

   (* Constructs a 16-bit bitstring with the first four bits containing
      the integer 1, and the following 12 bits containing the integer 10,
      arranged in network byte order. *)

Bitmatch.hexdump_bitstring stdout bits ;;

   (* Prints:

      00000000  10 0a         |..              |
    *)
]}

   


   {2:integertypes Integer types}






   {2 Compiling}





   {2 Safety and security considerations}




   {2 Limits}





   {2 Reference}
   {3 Types}
*)

type bitstring = string * int * int
(** [bitstring] is the basic type used to store bitstrings.

    The type contains the underlying data (a string),
    the current bit offset within the string and the
    current bit length of the string (counting from the
    bit offset).  Note that the offsets are bits, not bytes.

    Normally you don't need to use the bitstring type
    directly, since there are functions and syntax
    extensions which hide the details.
    See {!bitstring_of_file}, {!hexdump_bitstring},
    {!bitstring_length}.
*)

(** {3 Exceptions} *)

exception Construct_failure of string * string * int * int
(** [Construct_failure (message, file, line, char)] may be
    raised by the [BITSTRING] constructor.

    Common reasons are that values are out of range of
    the fields that contain them, or that computed lengths
    are impossible (eg. negative length bitfields).

    [message] is the error message.

    [file], [line] and [char] point to the original source
    location of the [BITSTRING] constructor that failed.
*)

(** {3 Bitstrings} *)

val empty_bitstring : bitstring
(** [empty_bitstring] is the empty, zero-length bitstring. *)

val create_bitstring : int -> bitstring
(** [create_bitstring n] creates an [n] bit bitstring
    containing all zeroes. *)

val make_bitstring : int -> char -> bitstring
(** [make_bitstring n c] creates an [n] bit bitstring
    containing the repeated 8 bit pattern in [c].

    For example, [make_bitstring 16 '\x5a'] will create
    the bitstring [0x5a5a] or in binary [0101 1010 0101 1010].

    Note that the length is in bits, not bytes. *)

val bitstring_of_chan : in_channel -> bitstring
(** [bitstring_of_chan chan] loads the contents of
    the input channel [chan] as a bitstring.

    The length of the final bitstring is determined
    by the remaining input in [chan], but will always
    be a multiple of 8 bits. *)

val bitstring_of_file : string -> bitstring
(** [bitstring_of_file filename] loads the named file
    into a bitstring. *)

val hexdump_bitstring : out_channel -> bitstring -> unit
(** [hexdump_bitstring chan bitstring] prints the bitstring
    to the output channel in a format similar to the
    Unix command [hexdump -C]. *)

val bitstring_length : bitstring -> int
(** [bitstring_length bitstring] returns the length of
    the bitstring in bits. *)

(** {3 Bitstring buffer} *)

module Buffer : sig
  type t
  val create : unit -> t
  val contents : t -> bitstring
  val add_bits : t -> string -> int -> unit
  val add_bit : t -> bool -> unit
  val add_byte : t -> int -> unit
end
(** Buffers are mainly used by the [BITSTRING] constructor, but
    may also be useful for end users.  They work much like the
    standard library [Buffer] module. *)

(** {3 Miscellaneous} *)

val debug : bool ref
(** Set this variable to true to enable extended debugging.
    This only works if debugging was also enabled in the
    [pa_bitmatch.ml] file at compile time, otherwise it
    does nothing. *)

(**/**)

(* Private functions, called from generated code.  Do not use
 * these directly - they are not safe.
 *)

val extract_bitstring : string -> int -> int -> int -> bitstring * int * int

val extract_remainder : string -> int -> int -> bitstring * int * int

val extract_bit : string -> int -> int -> int -> bool * int * int

val extract_char_unsigned : string -> int -> int -> int -> int * int * int

val extract_int_be_unsigned : string -> int -> int -> int -> int * int * int

val extract_int_le_unsigned : string -> int -> int -> int -> int * int * int

val extract_int32_be_unsigned : string -> int -> int -> int -> int32 * int * int

val extract_int32_le_unsigned : string -> int -> int -> int -> int32 * int * int

val extract_int64_be_unsigned : string -> int -> int -> int -> int64 * int * int

val construct_bit : Buffer.t -> bool -> int -> unit

val construct_char_unsigned : Buffer.t -> int -> int -> exn -> unit

val construct_int_be_unsigned : Buffer.t -> int -> int -> exn -> unit

val construct_int64_be_unsigned : Buffer.t -> int64 -> int -> exn -> unit
