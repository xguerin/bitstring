(* Bitmatch syntax extension.
 * Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
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
 * $Id: pa_bitmatch.ml,v 1.11 2008-04-25 14:57:11 rjones Exp $
 *)

open Printf

open Camlp4.PreCast
open Syntax
open Ast

(* If this is true then we emit some debugging code which can
 * be useful to tell what is happening during matches.  You
 * also need to do 'Bitmatch.debug := true' in your main program.
 *
 * If this is false then no extra debugging code is emitted.
 *)
let debug = false

(* A field when used in a bitmatch (a pattern). *)
type fpatt = {
  fpatt : patt;				(* field matching pattern *)
  fpc : fcommon;
}
(* A field when used in a BITSTRING constructor (an expression). *)
and fexpr = {
  fexpr : expr;				(* field value *)
  fec : fcommon;
}

and fcommon = {
  flen : expr;				(* length in bits, may be non-const *)
  endian : endian;			(* endianness *)
  signed : bool;			(* true if signed, false if unsigned *)
  t : t;				(* type *)
  _loc : Loc.t;				(* location in source code *)
}
and endian = BigEndian | LittleEndian | NativeEndian
and t = Int | String | Bitstring

(* Generate a fresh, unique symbol each time called. *)
let gensym =
  let i = ref 1000 in
  fun name ->
    incr i; let i = !i in
    sprintf "__pabitmatch_%s_%d" name i

let rec parse_patt_field _loc fpatt flen qs =
  let fpc = parse_field_common _loc flen qs in
  { fpatt = fpatt; fpc = fpc }

and parse_constr_field _loc fexpr flen qs =
  let fec = parse_field_common _loc flen qs in
  { fexpr = fexpr; fec = fec }

(* Deal with the qualifiers which appear for a field of both types. *)
and parse_field_common _loc flen qs =
  let endian, signed, t =
    match qs with
    | None -> (None, None, None)
    | Some qs ->
	List.fold_left (
	  fun (endian, signed, t) q ->
	    match q with
	    | "bigendian" ->
		if endian <> None then
		  Loc.raise _loc (Failure "an endian flag has been set already")
		else (
		  let endian = Some BigEndian in
		  (endian, signed, t)
		)
	    | "littleendian" ->
		if endian <> None then
		  Loc.raise _loc (Failure "an endian flag has been set already")
		else (
		  let endian = Some LittleEndian in
		  (endian, signed, t)
		)
	    | "nativeendian" ->
		if endian <> None then
		  Loc.raise _loc (Failure "an endian flag has been set already")
		else (
		  let endian = Some NativeEndian in
		  (endian, signed, t)
		)
	    | "signed" ->
		if signed <> None then
		  Loc.raise _loc (Failure "a signed flag has been set already")
		else (
		  let signed = Some true in
		  (endian, signed, t)
		)
	    | "unsigned" ->
		if signed <> None then
		  Loc.raise _loc (Failure "a signed flag has been set already")
		else (
		  let signed = Some false in
		  (endian, signed, t)
		)
	    | "int" ->
		if t <> None then
		  Loc.raise _loc (Failure "a type flag has been set already")
		else (
		  let t = Some Int in
		  (endian, signed, t)
		)
	    | "string" ->
		if t <> None then
		  Loc.raise _loc (Failure "a type flag has been set already")
		else (
		  let t = Some String in
		  (endian, signed, t)
		)
	    | "bitstring" ->
		if t <> None then
		  Loc.raise _loc (Failure "a type flag has been set already")
		else (
		  let t = Some Bitstring in
		  (endian, signed, t)
		)
	    | s ->
		Loc.raise _loc (Failure (s ^ ": unknown qualifier"))
	) (None, None, None) qs in

  (* If type is set to string or bitstring then endianness and
   * signedness qualifiers are meaningless and must not be set.
   *)
  if (t = Some Bitstring || t = Some String)
    && (endian <> None || signed <> None) then
      Loc.raise _loc (
	Failure "string types and endian or signed qualifiers cannot be mixed"
      );

  (* Default endianness, signedness, type. *)
  let endian = match endian with None -> BigEndian | Some e -> e in
  let signed = match signed with None -> false | Some s -> s in
  let t = match t with None -> Int | Some t -> t in

  {
    flen = flen;
    endian = endian;
    signed = signed;
    t = t;
    _loc = _loc;
  }

let string_of_endian = function
  | BigEndian -> "bigendian"
  | LittleEndian -> "littleendian"
  | NativeEndian -> "nativeendian"

let string_of_t = function
  | Int -> "int"
  | String -> "string"
  | Bitstring -> "bitstring"

let rec string_of_patt_field { fpatt = fpatt; fpc = fpc } =
  let fpc = string_of_field_common fpc in
  let fpatt =
    match fpatt with
    | <:patt< $lid:id$ >> -> id
    | _ -> "[pattern]" in
  fpatt ^ " : " ^ fpc

and string_of_constr_field { fexpr = fexpr; fec = fec } =
  let fec = string_of_field_common fec in
  let fexpr =
    match fexpr with
    | <:expr< $lid:id$ >> -> id
    | _ -> "[expression]" in
  fexpr ^ " : " ^ fec

and string_of_field_common { flen = flen;
			     endian = endian; signed = signed; t = t;
			     _loc = _loc } =
  let flen =
    match flen with
    | <:expr< $int:i$ >> -> i
    | _ -> "[non-const-len]" in
  let endian = string_of_endian endian in
  let signed = if signed then "signed" else "unsigned" in
  let t = string_of_t t in
  let loc_fname = Loc.file_name _loc in
  let loc_line = Loc.start_line _loc in
  let loc_char = Loc.start_off _loc - Loc.start_bol _loc in

  sprintf "%s : %s, %s, %s @ (%S, %d, %d)"
    flen t endian signed loc_fname loc_line loc_char

(* Generate the code for a constructor, ie. 'BITSTRING ...'. *)
let output_constructor _loc fields =
  let loc_fname = Loc.file_name _loc in
  let loc_line = string_of_int (Loc.start_line _loc) in
  let loc_char = string_of_int (Loc.start_off _loc - Loc.start_bol _loc) in

  (* Bitstrings are created like the 'Buffer' module (in fact, using
   * the Buffer module), by appending snippets to a growing buffer.
   * This is reasonably efficient and avoids a lot of garbage.
   *)
  let buffer = gensym "buffer" in

  (* General exception which is raised inside the constructor functions
   * when an int expression is out of range at runtime.
   *)
  let exn = gensym "exn" in
  let exn_used = ref false in

  (* Convert each field to a simple bitstring-generating expression. *)
  let fields = List.map (
    fun {fexpr=fexpr; fec={flen=flen; endian=endian; signed=signed;
			   t=t; _loc=_loc}} ->
      (* Is flen an integer constant?  If so, what is it?  This
       * is very simple-minded and only detects simple constants.
       *)
      let flen_is_const =
	match flen with
	| <:expr< $int:i$ >> -> Some (int_of_string i)
	| _ -> None in

      let name_of_int_construct_const = function
	  (* XXX As an enhancement we should allow a 64-bit-only
	   * mode which lets us use 'int' up to 63 bits and won't
	   * compile on 32-bit platforms.
	   *)
	  (* XXX The meaning of signed/unsigned breaks down at
	   * 31, 32, 63 and 64 bits.
	   *)
	| (1, _, _) -> "construct_bit"
	| ((2|3|4|5|6|7|8), _, false) -> "construct_char_unsigned"
	| ((2|3|4|5|6|7|8), _, true) -> "construct_char_signed"
	| (i, BigEndian, false) when i <= 31 -> "construct_int_be_unsigned"
	| (i, BigEndian, true) when i <= 31 -> "construct_int_be_signed"
	| (i, LittleEndian, false) when i <= 31 -> "construct_int_le_unsigned"
	| (i, LittleEndian, true) when i <= 31 -> "construct_int_le_signed"
	| (i, NativeEndian, false) when i <= 31 -> "construct_int_ne_unsigned"
	| (i, NativeEndian, true) when i <= 31 -> "construct_int_ne_signed"
	| (32, BigEndian, false) -> "construct_int32_be_unsigned"
	| (32, BigEndian, true) -> "construct_int32_be_signed"
	| (32, LittleEndian, false) -> "construct_int32_le_unsigned"
	| (32, LittleEndian, true) -> "construct_int32_le_signed"
	| (32, NativeEndian, false) -> "construct_int32_ne_unsigned"
	| (32, NativeEndian, true) -> "construct_int32_ne_signed"
	| (_, BigEndian, false) -> "construct_int64_be_unsigned"
	| (_, BigEndian, true) -> "construct_int64_be_signed"
	| (_, LittleEndian, false) -> "construct_int64_le_unsigned"
	| (_, LittleEndian, true) -> "construct_int64_le_signed"
	| (_, NativeEndian, false) -> "construct_int64_ne_unsigned"
	| (_, NativeEndian, true) -> "construct_int64_ne_signed"
      in
      let name_of_int_construct = function
	  (* XXX As an enhancement we should allow users to
	   * specify that a field length can fit into a char/int/int32
	   * (of course, this would have to be checked at runtime).
	   *)
	| (BigEndian, false) -> "construct_int64_be_unsigned"
	| (BigEndian, true) -> "construct_int64_be_signed"
	| (LittleEndian, false) -> "construct_int64_le_unsigned"
	| (LittleEndian, true) -> "construct_int64_le_signed"
	| (NativeEndian, false) -> "construct_int64_ne_unsigned"
	| (NativeEndian, true) -> "construct_int64_ne_signed"
      in

      let expr =
	match t, flen_is_const with
	(* Common case: int field, constant flen.
	 *
	 * Range checks are done inside the construction function
	 * because that's a lot simpler w.r.t. types.  It might
	 * be better to move them here. XXX
	 *)
	| Int, Some i when i > 0 && i <= 64 ->
	    let construct_func =
	      name_of_int_construct_const (i,endian,signed) in
	    exn_used := true;

	    <:expr<
	      Bitmatch.$lid:construct_func$ $lid:buffer$ $fexpr$ $flen$
	        $lid:exn$
	    >>

	| Int, Some _ ->
	    Loc.raise _loc (Failure "length of int field must be [1..64]")

	(* Int field, non-constant length.  We need to perform a runtime
	 * test to ensure the length is [1..64].
	 *
	 * Range checks are done inside the construction function
	 * because that's a lot simpler w.r.t. types.  It might
	 * be better to move them here. XXX
	 *)
	| Int, None ->
	    let construct_func = name_of_int_construct (endian,signed) in
	    exn_used := true;

	    <:expr<
	      if $flen$ >= 1 && $flen$ <= 64 then
		Bitmatch.$lid:construct_func$ $lid:buffer$ $fexpr$ $flen$
		  $lid:exn$
	      else
		raise (Bitmatch.Construct_failure
			 ("length of int field must be [1..64]",
			  $str:loc_fname$,
			  $int:loc_line$, $int:loc_char$))
	    >>

        (* String, constant length > 0, must be a multiple of 8. *)
	| String, Some i when i > 0 && i land 7 = 0 ->
	    let bs = gensym "bs" in
	    <:expr<
	      let $lid:bs$ = $fexpr$ in
	      if String.length $lid:bs$ = ($flen$ lsr 3) then
		Bitmatch.construct_string $lid:buffer$ $lid:bs$
	      else
		raise (Bitmatch.Construct_failure
			 ("length of string does not match declaration",
			  $str:loc_fname$,
			  $int:loc_line$, $int:loc_char$))
	    >>

	(* String, constant length -1, means variable length string
	 * with no checks.
	 *)
	| String, Some (-1) ->
	    <:expr< Bitmatch.construct_string $lid:buffer$ $fexpr$ >>

	(* String, constant length = 0 is probably an error, and so is
	 * any other value.
	 *)
	| String, Some _ ->
	    Loc.raise _loc (Failure "length of string must be > 0 and a multiple of 8, or the special value -1")

	(* String, non-constant length.
	 * We check at runtime that the length is > 0, a multiple of 8,
	 * and matches the declared length.
	 *)
	| String, None ->
	    let bslen = gensym "bslen" in
	    let bs = gensym "bs" in
	    <:expr<
	      let $lid:bslen$ = $flen$ in
	      if $lid:bslen$ > 0 then (
		if $lid:bslen$ land 7 = 0 then (
		  let $lid:bs$ = $fexpr$ in
		  if String.length $lid:bs$ = ($lid:bslen$ lsr 3) then
		    Bitmatch.construct_string $lid:buffer$ $lid:bs$
		  else
		    raise (Bitmatch.Construct_failure
			     ("length of string does not match declaration",
			      $str:loc_fname$,
			      $int:loc_line$, $int:loc_char$))
		) else
		  raise (Bitmatch.Construct_failure
			   ("length of string must be a multiple of 8",
			    $str:loc_fname$,
			    $int:loc_line$, $int:loc_char$))
	      ) else
		raise (Bitmatch.Construct_failure
			 ("length of string must be > 0",
			  $str:loc_fname$,
			  $int:loc_line$, $int:loc_char$))
	    >>

        (* Bitstring, constant length > 0. *)
	| Bitstring, Some i when i > 0 ->
	    let bs = gensym "bs" in
	    <:expr<
	      let $lid:bs$ = $fexpr$ in
	      if Bitmatch.bitstring_length $lid:bs$ = $flen$ then
		Bitmatch.construct_bitstring $lid:buffer$ $lid:bs$
	      else
		raise (Bitmatch.Construct_failure
			 ("length of bitstring does not match declaration",
			  $str:loc_fname$,
			  $int:loc_line$, $int:loc_char$))
	    >>

	(* Bitstring, constant length -1, means variable length bitstring
	 * with no checks.
	 *)
	| Bitstring, Some (-1) ->
	    <:expr< Bitmatch.construct_bitstring $lid:buffer$ $fexpr$ >>

	(* Bitstring, constant length = 0 is probably an error, and so is
	 * any other value.
	 *)
	| Bitstring, Some _ ->
	    Loc.raise _loc
	      (Failure
		 "length of bitstring must be > 0 or the special value -1")

	(* Bitstring, non-constant length.
	 * We check at runtime that the length is > 0 and matches
	 * the declared length.
	 *)
	| Bitstring, None ->
	    let bslen = gensym "bslen" in
	    let bs = gensym "bs" in
	    <:expr<
	      let $lid:bslen$ = $flen$ in
	      if $lid:bslen$ > 0 then (
		let $lid:bs$ = $fexpr$ in
		if Bitmatch.bitstring_length $lid:bs$ = $lid:bslen$ then
		  Bitmatch.construct_bitstring $lid:buffer$ $lid:bs$
		else
		  raise (Bitmatch.Construct_failure
			   ("length of bitstring does not match declaration",
			    $str:loc_fname$,
			    $int:loc_line$, $int:loc_char$))
	      ) else
		raise (Bitmatch.Construct_failure
			 ("length of bitstring must be > 0",
			  $str:loc_fname$,
			  $int:loc_line$, $int:loc_char$))
	    >> in
      expr
  ) fields in

  (* Create the final bitstring.  Start by creating an empty buffer
   * and then evaluate each expression above in turn which will
   * append some more to the bitstring buffer.  Finally extract
   * the bitstring.
   *
   * XXX We almost have enough information to be able to guess
   * a good initial size for the buffer.
   *)
  let fields =
    match fields with
    | [] -> <:expr< [] >>
    | h::t -> List.fold_left (fun h t -> <:expr< $h$; $t$ >>) h t in

  let expr =
    <:expr<
      let $lid:buffer$ = Bitmatch.Buffer.create () in
      $fields$;
      Bitmatch.Buffer.contents $lid:buffer$
    >> in

  if !exn_used then
    <:expr<
      let $lid:exn$ =
	Bitmatch.Construct_failure ("value out of range",
				    $str:loc_fname$,
				    $int:loc_line$, $int:loc_char$) in
	$expr$
    >>
  else
    expr

(* Generate the code for a bitmatch statement.  '_loc' is the
 * location, 'bs' is the bitstring parameter, 'cases' are
 * the list of cases to test against.
 *)
let output_bitmatch _loc bs cases =
  let data = gensym "data" and off = gensym "off" and len = gensym "len" in
  let result = gensym "result" in

  (* This generates the field extraction code for each
   * field a single case.  Each field must be wider than
   * the minimum permitted for the type and there must be
   * enough remaining data in the bitstring to satisfy it.
   * As we go through the fields, symbols 'data', 'off' and 'len'
   * track our position and remaining length in the bitstring.
   *
   * The whole thing is a lot of nested 'if' statements. Code
   * is generated from the inner-most (last) field outwards.
   *)
  let rec output_field_extraction inner = function
    | [] -> inner
    | field :: fields ->
	let {fpatt=fpatt; fpc={flen=flen; endian=endian; signed=signed;
			       t=t; _loc=_loc}}
	    = field in

	(* Is flen an integer constant?  If so, what is it?  This
	 * is very simple-minded and only detects simple constants.
	 *)
	let flen_is_const =
	  match flen with
	  | <:expr< $int:i$ >> -> Some (int_of_string i)
	  | _ -> None in

	let name_of_int_extract_const = function
	    (* XXX As an enhancement we should allow a 64-bit-only
	     * mode which lets us use 'int' up to 63 bits and won't
	     * compile on 32-bit platforms.
	     *)
	    (* XXX The meaning of signed/unsigned breaks down at
	     * 31, 32, 63 and 64 bits.
	     *)
	  | (1, _, _) -> "extract_bit"
	  | ((2|3|4|5|6|7|8), _, false) -> "extract_char_unsigned"
	  | ((2|3|4|5|6|7|8), _, true) -> "extract_char_signed"
	  | (i, BigEndian, false) when i <= 31 -> "extract_int_be_unsigned"
	  | (i, BigEndian, true) when i <= 31 -> "extract_int_be_signed"
	  | (i, LittleEndian, false) when i <= 31 -> "extract_int_le_unsigned"
	  | (i, LittleEndian, true) when i <= 31 -> "extract_int_le_signed"
	  | (i, NativeEndian, false) when i <= 31 -> "extract_int_ne_unsigned"
	  | (i, NativeEndian, true) when i <= 31 -> "extract_int_ne_signed"
	  | (32, BigEndian, false) -> "extract_int32_be_unsigned"
	  | (32, BigEndian, true) -> "extract_int32_be_signed"
	  | (32, LittleEndian, false) -> "extract_int32_le_unsigned"
	  | (32, LittleEndian, true) -> "extract_int32_le_signed"
	  | (32, NativeEndian, false) -> "extract_int32_ne_unsigned"
	  | (32, NativeEndian, true) -> "extract_int32_ne_signed"
	  | (_, BigEndian, false) -> "extract_int64_be_unsigned"
	  | (_, BigEndian, true) -> "extract_int64_be_signed"
	  | (_, LittleEndian, false) -> "extract_int64_le_unsigned"
	  | (_, LittleEndian, true) -> "extract_int64_le_signed"
	  | (_, NativeEndian, false) -> "extract_int64_ne_unsigned"
	  | (_, NativeEndian, true) -> "extract_int64_ne_signed"
	in
	let name_of_int_extract = function
	    (* XXX As an enhancement we should allow users to
	     * specify that a field length can fit into a char/int/int32
	     * (of course, this would have to be checked at runtime).
	     *)
	  | (BigEndian, false) -> "extract_int64_be_unsigned"
	  | (BigEndian, true) -> "extract_int64_be_signed"
	  | (LittleEndian, false) -> "extract_int64_le_unsigned"
	  | (LittleEndian, true) -> "extract_int64_le_signed"
	  | (NativeEndian, false) -> "extract_int64_ne_unsigned"
	  | (NativeEndian, true) -> "extract_int64_ne_signed"
	in

	let expr =
	  match t, flen_is_const with
	  (* Common case: int field, constant flen *)
	  | Int, Some i when i > 0 && i <= 64 ->
	      let extract_func = name_of_int_extract_const (i,endian,signed) in
	      let v = gensym "val" in
	      <:expr<
		if $lid:len$ >= $flen$ then (
		  let $lid:v$, $lid:off$, $lid:len$ =
		    Bitmatch.$lid:extract_func$ $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  match $lid:v$ with $fpatt$ when true -> $inner$ | _ -> ()
		)
	      >>

	  | Int, Some _ ->
	      Loc.raise _loc (Failure "length of int field must be [1..64]")

	  (* Int field, non-const flen.  We have to test the range of
	   * the field at runtime.  If outside the range it's a no-match
	   * (not an error).
	   *)
	  | Int, None ->
	      let extract_func = name_of_int_extract (endian,signed) in
	      let v = gensym "val" in
	      <:expr<
		if $flen$ >= 1 && $flen$ <= 64 && $flen$ <= $lid:len$ then (
		  let $lid:v$, $lid:off$, $lid:len$ =
		    Bitmatch.$lid:extract_func$ $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  match $lid:v$ with $fpatt$ when true -> $inner$ | _ -> ()
		)
	      >>

          (* String, constant flen > 0. *)
	  | String, Some i when i > 0 && i land 7 = 0 ->
	      let bs = gensym "bs" in
	      <:expr<
		if $lid:len$ >= $flen$ then (
		  let $lid:bs$, $lid:off$, $lid:len$ =
		    Bitmatch.extract_bitstring $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  match Bitmatch.string_of_bitstring $lid:bs$ with
		  | $fpatt$ when true -> $inner$
		  | _ -> ()
		)
	      >>

          (* String, constant flen = -1, means consume all the
	   * rest of the input.
	   *)
	  | String, Some i when i = -1 ->
	      let bs = gensym "bs" in
	      <:expr<
		let $lid:bs$, $lid:off$, $lid:len$ =
		  Bitmatch.extract_remainder $lid:data$ $lid:off$ $lid:len$ in
		match Bitmatch.string_of_bitstring $lid:bs$ with
		| $fpatt$ when true -> $inner$
		| _ -> ()
	      >>

	  | String, Some _ ->
	      Loc.raise _loc (Failure "length of string must be > 0 and a multiple of 8, or the special value -1")

	  (* String field, non-const flen.  We check the flen is > 0
	   * and a multiple of 8 (-1 is not allowed here), at runtime.
	   *)
	  | String, None ->
	      let bs = gensym "bs" in
	      <:expr<
		if $flen$ >= 0 && $flen$ <= $lid:len$
		  && $flen$ land 7 = 0 then (
		    let $lid:bs$, $lid:off$, $lid:len$ =
		      Bitmatch.extract_bitstring
			$lid:data$ $lid:off$ $lid:len$ $flen$ in
		    match Bitmatch.string_of_bitstring $lid:bs$ with
		    | $fpatt$ when true -> $inner$
		    | _ -> ()
		  )
	      >>

          (* Bitstring, constant flen >= 0.
	   * At the moment all we can do is assign the bitstring to an
	   * identifier.
	   *)
	  | Bitstring, Some i when i >= 0 ->
	      let ident =
		match fpatt with
		| <:patt< $lid:ident$ >> -> ident
		| <:patt< _ >> -> "_"
		| _ ->
		    Loc.raise _loc
		      (Failure "cannot compare a bitstring to a constant") in
	      <:expr<
		if $lid:len$ >= $flen$ then (
		  let $lid:ident$, $lid:off$, $lid:len$ =
		    Bitmatch.extract_bitstring $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  $inner$
		)
	      >>

          (* Bitstring, constant flen = -1, means consume all the
	   * rest of the input.
	   *)
	  | Bitstring, Some i when i = -1 ->
	      let ident =
		match fpatt with
		| <:patt< $lid:ident$ >> -> ident
		| _ ->
		    Loc.raise _loc
		      (Failure "cannot compare a bitstring to a constant") in
	      <:expr<
		let $lid:ident$, $lid:off$, $lid:len$ =
		  Bitmatch.extract_remainder $lid:data$ $lid:off$ $lid:len$ in
		  $inner$
	      >>

	  | Bitstring, Some _ ->
	      Loc.raise _loc (Failure "length of bitstring must be >= 0 or the special value -1")

	  (* Bitstring field, non-const flen.  We check the flen is >= 0
	   * (-1 is not allowed here) at runtime.
	   *)
	  | Bitstring, None ->
	      let ident =
		match fpatt with
		| <:patt< $lid:ident$ >> -> ident
		| _ ->
		    Loc.raise _loc
		      (Failure "cannot compare a bitstring to a constant") in
	      <:expr<
		if $flen$ >= 0 && $flen$ <= $lid:len$ then (
		  let $lid:ident$, $lid:off$, $lid:len$ =
		    Bitmatch.extract_bitstring $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  $inner$
		)
	      >>
	in

	(* Emit extra debugging code. *)
	let expr =
	  if not debug then expr else (
	    let field = string_of_patt_field field in

	    <:expr<
	      if !Bitmatch.debug then (
		Printf.eprintf "PA_BITMATCH: TEST:\n";
		Printf.eprintf "  %s\n" $str:field$;
		Printf.eprintf "  off %d len %d\n%!" $lid:off$ $lid:len$;
		(*Bitmatch.hexdump_bitstring stderr
		  ($lid:data$,$lid:off$,$lid:len$);*)
	      );
	      $expr$
	    >>
	  ) in

	output_field_extraction expr fields
  in

  (* Convert each case in the match. *)
  let cases = List.map (
    fun (fields, bind, whenclause, code) ->
      let inner = <:expr< $lid:result$ := Some ($code$); raise Exit >> in
      let inner =
	match whenclause with
	| Some whenclause ->
	    <:expr< if $whenclause$ then $inner$ >>
	| None -> inner in
      let inner =
	match bind with
	| Some name ->
	    <:expr<
	      let $lid:name$ = ($lid:data$, $lid:off$, $lid:len$) in
	      $inner$
	      >>
	| None -> inner in
      output_field_extraction inner (List.rev fields)
  ) cases in

  (* Join them into a single expression.
   *
   * Don't do it with a normal fold_right because that leaves
   * 'raise Exit; ()' at the end which causes a compiler warning.
   * Hence a bit of complexity here.
   *
   * Note that the number of cases is always >= 1 so List.hd is safe.
   *)
  let cases = List.rev cases in
  let cases =
    List.fold_left (fun base case -> <:expr< $case$ ; $base$ >>)
      (List.hd cases) (List.tl cases) in

  (* The final code just wraps the list of cases in a
   * try/with construct so that each case is tried in
   * turn until one case matches (that case sets 'result'
   * and raises 'Exit' to leave the whole statement).
   * If result isn't set by the end then we will raise
   * Match_failure with the location of the bitmatch
   * statement in the original code.
   *)
  let loc_fname = Loc.file_name _loc in
  let loc_line = string_of_int (Loc.start_line _loc) in
  let loc_char = string_of_int (Loc.start_off _loc - Loc.start_bol _loc) in

  <:expr<
    let ($lid:data$, $lid:off$, $lid:len$) = $bs$ in
    let $lid:result$ = ref None in
    (try
      $cases$
    with Exit -> ());
    match ! $lid:result$ with
    | Some x -> x
    | None -> raise (Match_failure ($str:loc_fname$,
				    $int:loc_line$, $int:loc_char$))
  >>

EXTEND Gram
  GLOBAL: expr;

  qualifiers: [
    [ LIST0 [ q = LIDENT -> q ] SEP "," ]
  ];

  (* Field used in the bitmatch operator (a pattern). *)
  patt_field: [
    [ fpatt = patt; ":"; len = expr LEVEL "top";
      qs = OPT [ ":"; qs = qualifiers -> qs ] ->
	parse_patt_field _loc fpatt len qs
    ]
  ];

  (* Case inside bitmatch operator. *)
  match_case: [
    [ "{";
      fields = LIST0 patt_field SEP ";";
      "}";
      bind = OPT [ "as"; name = LIDENT -> name ];
      whenclause = OPT [ "when"; e = expr -> e ]; "->";
      code = expr ->
	(fields, bind, whenclause, code)
    ]
  ];

  (* Field used in the BITSTRING constructor (an expression). *)
  constr_field: [
    [ fexpr = expr LEVEL "top"; ":"; len = expr LEVEL "top";
      qs = OPT [ ":"; qs = qualifiers -> qs ] ->
	parse_constr_field _loc fexpr len qs
    ]
  ];

  (* 'bitmatch' expressions. *)
  expr: LEVEL ";" [
    [ "bitmatch";
      bs = expr; "with"; OPT "|";
      cases = LIST1 match_case SEP "|" ->
	output_bitmatch _loc bs cases
    ]

  (* Constructor. *)
  | [ "BITSTRING"; "{";
      fields = LIST0 constr_field SEP ";";
      "}" ->
	output_constructor _loc fields
    ]
  ];

END
