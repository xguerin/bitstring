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
 * $Id: bitmatch.mli,v 1.8 2008-04-02 08:14:40 rjones Exp $
 *)

(**
   {2 Introduction and examples}


   {2 Reference}

   {3 Types}
*)

type bitstring = string * int * int

(** {3 Exceptions} *)

exception Construct_failure of string * string * int * int

(** {3 Bitstrings} *)

val empty_bitstring : bitstring

val create_bitstring : int -> bitstring

val make_bitstring : int -> char -> bitstring

val bitstring_of_chan : in_channel -> bitstring

val bitstring_of_file : string -> bitstring

val hexdump_bitstring : out_channel -> bitstring -> unit

val bitstring_length : bitstring -> int

(** {3 Bitstring buffer} *)

module Buffer : sig
  type t
  val create : unit -> t
  val contents : t -> bitstring
  val add_bits : t -> string -> int -> unit
  val add_bit : t -> bool -> unit
  val add_byte : t -> int -> unit
end

(** {3 Miscellaneous} *)

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
