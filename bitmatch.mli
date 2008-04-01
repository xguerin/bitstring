(* Bitmatch library.
 * $Id: bitmatch.mli,v 1.3 2008-04-01 10:06:12 rjones Exp $
 *)

type bitstring = string * int * int

val empty_bitstring : bitstring

val create_bitstring : int -> bitstring

val make_bitstring : int -> char -> bitstring

val bitstring_of_chan : in_channel -> bitstring

val bitstring_of_file : string -> bitstring

val hexdump_bitstring : out_channel -> bitstring -> unit

(**/**)

val extract_bitstring : string -> int -> int -> int -> bitstring * int * int

val extract_remainder : string -> int -> int -> bitstring * int * int

val extract_bit : string -> int -> int -> int -> bool * int * int

val extract_char_unsigned : string -> int -> int -> int -> int * int * int
