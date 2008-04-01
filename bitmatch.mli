(* Bitmatch library.
 * $Id: bitmatch.mli,v 1.6 2008-04-01 19:10:45 rjones Exp $
 *)

exception Construct_failure of string * string * int * int

type bitstring = string * int * int

val empty_bitstring : bitstring

val create_bitstring : int -> bitstring

val make_bitstring : int -> char -> bitstring

val bitstring_of_chan : in_channel -> bitstring

val bitstring_of_file : string -> bitstring

val hexdump_bitstring : out_channel -> bitstring -> unit

val bitstring_length : bitstring -> int

module Buffer : sig
  type t
  val create : unit -> t
  val contents : t -> bitstring
  val add_bits : t -> string -> int -> unit
  val add_bit : t -> bool -> unit
  val add_byte : t -> int -> unit
end

val debug : bool ref

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

val construct_int64_be_unsigned : Buffer.t -> int64 -> int -> exn -> unit
