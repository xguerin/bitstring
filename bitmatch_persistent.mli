(** Bitmatch persistent patterns. *)
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
 * $Id$
 *)

(**
   {{:#reference}Jump straight to the reference section for
   documentation on types and functions}.

   {2 Introduction}

   Bitmatch allows you to name sets of fields and reuse them
   elsewhere.  For example if you frequently need to parse
   Pascal-style strings in the form [length byte + string], then you
   could name the [{ strlen : 8 : int; str : strlen*8 : string}]
   pattern and reuse it everywhere by name.

   These are called {b persistent patterns}.

   The basic usage is:

{v
(* Create a persistent pattern called 'pascal' which
 * matches Pascal-style strings (length byte + string).
 *)
bitmatch pascal =
  { strlen : 8 : int;
    str : strlen*8 : string }

let is_pascal_string bits =
  bitmatch bits with
  | { pascal } ->
    printf "matches a Pascal string %s, len %d bytes\n"
      str strlen
v}

   {3 Important notes}

   There are some important things you should know about
   persistent patterns before you decide to use them:

   'Persistent' refers to the fact that they can be saved into binary
   files.  However these binary files use OCaml [Marshal] module and
   depend (sometimes) on the version of OCaml used to generate them
   and (sometimes) the version of bitmatch used.  So your build system
   should rebuild these files from source when your code is rebuilt.

   Persistent patterns are syntactic.  They work in the same way
   as cutting and pasting (or [#include]-ing) code.  For example
   if a persistent pattern binds a field named [len], then any
   uses of [len] following in the surrounding pattern could
   be affected.

   Programs which generate and manipulate persistent patterns have to
   link to camlp4.  Since camlp4 in OCaml >= 3.10 is rather large, we
   have placed this code into this separate submodule, so that
   programs which just use bitmatch don't need to pull in the whole of
   camlp4.  This restriction does not apply to generated code which
   only uses persistent patterns.  If the distinction isn't clear,
   use [ocamlobjinfo] to look at the dependencies of your [*.cmo]
   files.

   Persistent patterns can be generated in several ways, but they
   can only be {i used} by the [pa_bitmatch] syntax extension.
   This means they are purely compile-time constructs.  You
   cannot use them to make arbitrary patterns and run those
   patterns (not unless your program runs [ocamlc] to make a [*.cmo]
   file then dynamically links to the [*.cmo] file).










   {2:reference Reference}

   {3 Internal}
*)

type patt = Camlp4.PreCast.Syntax.Ast.patt
type expr = Camlp4.PreCast.Syntax.Ast.expr
type loc_t = Camlp4.PreCast.Syntax.Ast.Loc.t

(** {3 Types} *)

type 'a field
(** A field in a persistent pattern or persistent constructor. *)

type pattern = patt field list
(** A persistent pattern (used in [bitmatch] operator), is just a list
    of pattern fields. *)

type constructor = expr field list
(** A persistent constructor (used in [BITSTRING] operator), is just a
    list of constructor fields. *)

(** {3 Printers} *)

val string_of_pattern : pattern -> string
val string_of_constructor : constructor -> string
val string_of_field : 'a field -> string
(** Convert patterns, constructors, or individual fields
    into printable strings for debugging purposes.

    The strings look similar to the syntax used by bitmatch, but
    some things cannot be printed fully, eg. length expressions. *)

(** {3 Persistence} *)

val pattern_to_channel : out_channel -> pattern -> unit
val constructor_to_channel : out_channel -> constructor -> unit
(** Save a pattern/constructor to an output channel. *)

val pattern_to_string : pattern -> string
val constructor_to_string : constructor -> string
(** Serialize a pattern/constructor to a string. *)

val pattern_to_buffer : string -> int -> int -> pattern -> int
val constructor_to_buffer : string -> int -> int -> constructor -> int
(** Serialize a pattern/constructor to part of a string, return the length. *)

val pattern_from_channel : in_channel -> pattern
val constructor_from_channel : in_channel -> constructor
(** Load a pattern/constructor from an output channel.

    Note: This is not type safe.  The pattern/constructor must
    have been written out under the same version of OCaml and
    the same version of bitmatch. *)

val pattern_from_string : string -> int -> pattern
val constructor_from_string : string -> int -> constructor
(** Load a pattern/constructor from a string at offset within the string.

    Note: This is not type safe.  The pattern/constructor must
    have been written out under the same version of OCaml and
    the same version of bitmatch. *)

(** {3 Create pattern fields}

    These fields are used in pattern matches ([bitmatch]). *)

val create_pattern_field : loc_t -> patt field
(** Create a pattern field.

    The pattern is unbound, the type is set to [int], bit length to [32],
    endianness to [BigEndian], signedness to unsigned ([false]),
    and source code location to the [_loc] parameter.

    To create a complete field you need to call the [set_*]
    functions.  For example, to create [{ len : 8 : int }]
    you would do:

{v
    let field = create_pattern_field _loc in
    let field = set_lident_patt field "len" in
    let field = set_length_int field 8 in
v}
*)

val set_lident_patt : patt field -> string -> patt field
(** Sets the pattern to the pattern binding an identifier
    given in the string.

    The effect is that the field [{ len : 8 : int }] could
    be created by calling [set_lident_patt field "len"]. *)

val set_int_patt : patt field -> int -> patt field
(** Sets the pattern field to the pattern which matches an integer.

    The effect is that the field [{ 2 : 8 : int }] could
    be created by calling [set_int_patt field 2]. *)

val set_string_patt : patt field -> string -> patt field
(** Sets the pattern field to the pattern which matches a string.

    The effect is that the field [{ "MAGIC" : 8*5 : string }] could
    be created by calling [set_int_patt field "MAGIC"]. *)

val set_unbound_patt : patt field -> patt field
(** Sets the pattern field to the unbound pattern (usually written [_]).

    The effect is that the field [{ _ : 8 : int }] could
    be created by calling [set_unbound_patt field]. *)

val set_patt : patt field -> patt -> patt field
(** Sets the pattern field to an arbitrary OCaml pattern match. *)

val set_length_int : 'a field -> int -> 'a field
(** Sets the length in bits of a field to a constant integer.

    The effect is that the field [{ len : 8 : string }] could
    be created by calling [set_length field 8]. *)

val set_length : 'a field -> expr -> 'a field
(** Sets the length in bits of a field to an OCaml expression.

    The effect is that the field [{ len : 2*i : string }] could
    be created by calling [set_length field <:expr< 2*i >>]. *)

val set_endian : 'a field -> Bitmatch.endian -> 'a field
(** Sets the endianness of a field to the constant endianness.

    The effect is that the field [{ _ : 16 : bigendian }] could
    be created by calling [set_endian field Bitmatch.BigEndian]. *)

val set_endian_expr : 'a field -> expr -> 'a field
(** Sets the endianness of a field to an endianness expression.

    The effect is that the field [{ _ : 16 : endian(e) }] could
    be created by calling [set_endian_expr field e]. *)

val set_signed : 'a field -> bool -> 'a field
(** Sets the signedness of a field to a constant signedness.

    The effect is that the field [{ _ : 16 : signed }] could
    be created by calling [set_signed field true]. *)

val set_type_int : 'a field -> 'a field
(** Sets the type of a field to [int].

    The effect is that the field [{ _ : 16 : int }] could
    be created by calling [set_type_int field]. *)

val set_type_string : 'a field -> 'a field
(** Sets the type of a field to [string].

    The effect is that the field [{ str : 16 : string }] could
    be created by calling [set_type_string field]. *)

val set_type_bitstring : 'a field -> 'a field
(** Sets the type of a field to [bitstring].

    The effect is that the field [{ _ : 768 : bitstring }] could
    be created by calling [set_type_bitstring field]. *)

val set_location : 'a field -> loc_t -> 'a field
(** Sets the source code location of a field.  This is used when
    pa_bitmatch displays error messages. *)

(** {3 Create constructor fields}

    These fields are used in constructors ([BITSTRING]). *)

val create_constructor_field : loc_t -> expr field
(** Create a constructor field.

    The defaults are the same as for {!create_pattern_field}
    except that the expression is initialized to [0].
*)

val set_lident_expr : expr field -> string -> expr field
(** Sets the expression in a constructor field to an expression
    which uses the identifier.

    The effect is that the field [{ len : 8 : int }] could
    be created by calling [set_lident_expr field "len"]. *)

val set_int_expr : expr field -> int -> expr field
(** Sets the expression to the value of the integer.

    The effect is that the field [{ 2 : 8 : int }] could
    be created by calling [set_int_expr field 2]. *)

val set_string_expr : expr field -> string -> expr field
(** Sets the expression to the value of the string.

    The effect is that the field [{ "MAGIC" : 8*5 : string }] could
    be created by calling [set_int_expr field "MAGIC"]. *)

val set_expr : expr field -> expr -> expr field
(** Sets the expression field to an arbitrary OCaml expression. *)

(** {3 Accessors} *)

val get_patt : patt field -> patt
(** Get the pattern from a pattern field. *)

val get_expr : expr field -> expr
(** Get the expression from an expression field. *)

val get_length : 'a field -> expr
(** Get the length in bits from a field.  Note that what is returned
    is an OCaml expression, since lengths can be non-constant. *)

type endian_expr =
  | ConstantEndian of Bitmatch.endian
  | EndianExpr of expr

val get_endian : 'a field -> endian_expr
(** Get the endianness of a field.  This is an {!endian_expr} which
    could be a constant or an OCaml expression. *)

val get_signed : 'a field -> bool
(** Get the signedness of a field. *)

type field_type = Int | String | Bitstring

val get_type : 'a field -> field_type
(** Get the type of a field, [Int], [String] or [Bitstring]. *)

val get_location : 'a field -> loc_t
(** Get the source code location of a field. *)
