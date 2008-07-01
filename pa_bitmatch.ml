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
 * $Id$
 *)

open Printf

open Camlp4.PreCast
open Syntax
open Ast

open Bitmatch
module P = Bitmatch_persistent

(* If this is true then we emit some debugging code which can
 * be useful to tell what is happening during matches.  You
 * also need to do 'Bitmatch.debug := true' in your main program.
 *
 * If this is false then no extra debugging code is emitted.
 *)
let debug = false

(* Hashtable storing named persistent patterns. *)
let pattern_hash : (string, P.pattern) Hashtbl.t = Hashtbl.create 13

let locfail _loc msg = Loc.raise _loc (Failure msg)

(* Work out if an expression is an integer constant.
 *
 * Returns [Some i] if so (where i is the integer value), else [None].
 *
 * Fairly simplistic algorithm: we can only detect simple constant
 * expressions such as [k], [k+c], [k-c] etc.
 *)
let rec expr_is_constant = function
  | <:expr< $int:i$ >> ->		(* Literal integer constant. *)
    Some (int_of_string i)
  | <:expr< $a$ + $b$ >> ->		(* Addition of constants. *)
    (match expr_is_constant a, expr_is_constant b with
     | Some a, Some b -> Some (a+b)
     | _ -> None)
  | <:expr< $a$ - $b$ >> ->		(* Subtraction. *)
    (match expr_is_constant a, expr_is_constant b with
     | Some a, Some b -> Some (a-b)
     | _ -> None)
  | <:expr< $a$ * $b$ >> ->	        (* Multiplication. *)
    (match expr_is_constant a, expr_is_constant b with
     | Some a, Some b -> Some (a*b)
     | _ -> None)
  | <:expr< $a$ / $b$ >> ->	        (* Division. *)
    (match expr_is_constant a, expr_is_constant b with
     | Some a, Some b -> Some (a/b)
     | _ -> None)
  | <:expr< $a$ lsl $b$ >> ->	        (* Shift left. *)
    (match expr_is_constant a, expr_is_constant b with
     | Some a, Some b -> Some (a lsl b)
     | _ -> None)
  | <:expr< $a$ lsr $b$ >> ->	        (* Shift right. *)
    (match expr_is_constant a, expr_is_constant b with
     | Some a, Some b -> Some (a lsr b)
     | _ -> None)
  | _ -> None				(* Anything else is not constant. *)

(* Generate a fresh, unique symbol each time called. *)
let gensym =
  let i = ref 1000 in
  fun name ->
    incr i; let i = !i in
    sprintf "__pabitmatch_%s_%d" name i

(* Deal with the qualifiers which appear for a field of both types. *)
let parse_field _loc field qs =
  let fail = locfail _loc in

  let endian_set, signed_set, type_set, offset_set, field =
    match qs with
    | None -> (false, false, false, false, field)
    | Some qs ->
	List.fold_left (
	  fun (endian_set, signed_set, type_set, offset_set, field) qual_expr ->
	    match qual_expr with
	    | "bigendian", None ->
		if endian_set then
		  fail "an endian flag has been set already"
		else (
		  let field = P.set_endian field BigEndian in
		  (true, signed_set, type_set, offset_set, field)
		)
	    | "littleendian", None ->
		if endian_set then
		  fail "an endian flag has been set already"
		else (
		  let field = P.set_endian field LittleEndian in
		  (true, signed_set, type_set, offset_set, field)
		)
	    | "nativeendian", None ->
		if endian_set then
		  fail "an endian flag has been set already"
		else (
		  let field = P.set_endian field NativeEndian in
		  (true, signed_set, type_set, offset_set, field)
		)
	    | "endian", Some expr ->
		if endian_set then
		  fail "an endian flag has been set already"
		else (
		  let field = P.set_endian_expr field expr in
		  (true, signed_set, type_set, offset_set, field)
		)
	    | "signed", None ->
		if signed_set then
		  fail "a signed flag has been set already"
		else (
		  let field = P.set_signed field true in
		  (endian_set, true, type_set, offset_set, field)
		)
	    | "unsigned", None ->
		if signed_set then
		  fail "a signed flag has been set already"
		else (
		  let field = P.set_signed field false in
		  (endian_set, true, type_set, offset_set, field)
		)
	    | "int", None ->
		if type_set then
		  fail "a type flag has been set already"
		else (
		  let field = P.set_type_int field in
		  (endian_set, signed_set, true, offset_set, field)
		)
	    | "string", None ->
		if type_set then
		  fail "a type flag has been set already"
		else (
		  let field = P.set_type_string field in
		  (endian_set, signed_set, true, offset_set, field)
		)
	    | "bitstring", None ->
		if type_set then
		  fail "a type flag has been set already"
		else (
		  let field = P.set_type_bitstring field in
		  (endian_set, signed_set, true, offset_set, field)
		)
	    | "offset", Some expr ->
		if offset_set then
		  fail "an offset has been set already"
		else (
		  let field = P.set_offset field expr in
		  (endian_set, signed_set, type_set, true, field)
		)
	    | s, Some _ ->
		fail (s ^ ": unknown qualifier, or qualifier should not be followed by an expression")
	    | s, None ->
		fail (s ^ ": unknown qualifier, or qualifier should be followed by an expression")
	) (false, false, false, false, field) qs in

  (* If type is set to string or bitstring then endianness and
   * signedness qualifiers are meaningless and must not be set.
   *)
  let () =
    let t = P.get_type field in
    if (t = P.Bitstring || t = P.String) && (endian_set || signed_set) then
      fail "string types and endian or signed qualifiers cannot be mixed" in

  (* Default endianness, signedness, type if not set already. *)
  let field = if endian_set then field else P.set_endian field BigEndian in
  let field = if signed_set then field else P.set_signed field false in
  let field = if type_set then field else P.set_type_int field in

  field

(* Generate the code for a constructor, ie. 'BITSTRING ...'. *)
let output_constructor _loc fields =
  let fail = locfail _loc in

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
    fun field ->
      let fexpr = P.get_expr field in
      let flen = P.get_length field in
      let endian = P.get_endian field in
      let signed = P.get_signed field in
      let t = P.get_type field in
      let _loc = P.get_location field in
      let offset = P.get_offset field in

      (* offset() not supported in constructors.  Implementation of
       * forward-only offsets is fairly straightforward: we would
       * need to just calculate the length of padding here and add
       * it to what has been constructed.  For general offsets,
       * including going backwards, that would require a rethink in
       * how we construct bitstrings.
       *)
      if offset <> None then
	fail "offset expressions are not supported in BITSTRING constructors";

      (* Is flen an integer constant?  If so, what is it?  This
       * is very simple-minded and only detects simple constants.
       *)
      let flen_is_const = expr_is_constant flen in

      (* Choose the right constructor function. *)
      let int_construct_const = function
	  (* XXX The meaning of signed/unsigned breaks down at
	   * 31, 32, 63 and 64 bits.
	   *)
	| (1, _, _) ->
	    <:expr<Bitmatch.construct_bit>>
	| ((2|3|4|5|6|7|8), _, false) ->
	    <:expr<Bitmatch.construct_char_unsigned>>
	| ((2|3|4|5|6|7|8), _, true) ->
	    <:expr<Bitmatch.construct_char_signed>>
	| (i, P.ConstantEndian BigEndian, false) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_be_unsigned>>
	| (i, P.ConstantEndian BigEndian, true) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_be_signed>>
	| (i, P.ConstantEndian LittleEndian, false) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_le_unsigned>>
	| (i, P.ConstantEndian LittleEndian, true) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_le_signed>>
	| (i, P.ConstantEndian NativeEndian, false) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_ne_unsigned>>
	| (i, P.ConstantEndian NativeEndian, true) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_ne_signed>>
	| (i, P.EndianExpr expr, false) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_ee_unsigned $expr$>>
	| (i, P.EndianExpr expr, true) when i <= 31 ->
	    <:expr<Bitmatch.construct_int_ee_signed $expr$>>
	| (32, P.ConstantEndian BigEndian, false) ->
	    <:expr<Bitmatch.construct_int32_be_unsigned>>
	| (32, P.ConstantEndian BigEndian, true) ->
	    <:expr<Bitmatch.construct_int32_be_signed>>
	| (32, P.ConstantEndian LittleEndian, false) ->
	    <:expr<Bitmatch.construct_int32_le_unsigned>>
	| (32, P.ConstantEndian LittleEndian, true) ->
	    <:expr<Bitmatch.construct_int32_le_signed>>
	| (32, P.ConstantEndian NativeEndian, false) ->
	    <:expr<Bitmatch.construct_int32_ne_unsigned>>
	| (32, P.ConstantEndian NativeEndian, true) ->
	    <:expr<Bitmatch.construct_int32_ne_signed>>
	| (32, P.EndianExpr expr, false) ->
	    <:expr<Bitmatch.construct_int32_ee_unsigned $expr$>>
	| (32, P.EndianExpr expr, true) ->
	    <:expr<Bitmatch.construct_int32_ee_signed $expr$>>
	| (_, P.ConstantEndian BigEndian, false) ->
	    <:expr<Bitmatch.construct_int64_be_unsigned>>
	| (_, P.ConstantEndian BigEndian, true) ->
	    <:expr<Bitmatch.construct_int64_be_signed>>
	| (_, P.ConstantEndian LittleEndian, false) ->
	    <:expr<Bitmatch.construct_int64_le_unsigned>>
	| (_, P.ConstantEndian LittleEndian, true) ->
	    <:expr<Bitmatch.construct_int64_le_signed>>
	| (_, P.ConstantEndian NativeEndian, false) ->
	    <:expr<Bitmatch.construct_int64_ne_unsigned>>
	| (_, P.ConstantEndian NativeEndian, true) ->
	    <:expr<Bitmatch.construct_int64_ne_signed>>
	| (_, P.EndianExpr expr, false) ->
	    <:expr<Bitmatch.construct_int64_ee_unsigned $expr$>>
	| (_, P.EndianExpr expr, true) ->
	    <:expr<Bitmatch.construct_int64_ee_signed $expr$>>
      in
      let int_construct = function
	| (P.ConstantEndian BigEndian, false) ->
	    <:expr<Bitmatch.construct_int64_be_unsigned>>
	| (P.ConstantEndian BigEndian, true) ->
	    <:expr<Bitmatch.construct_int64_be_signed>>
	| (P.ConstantEndian LittleEndian, false) ->
	    <:expr<Bitmatch.construct_int64_le_unsigned>>
	| (P.ConstantEndian LittleEndian, true) ->
	    <:expr<Bitmatch.construct_int64_le_signed>>
	| (P.ConstantEndian NativeEndian, false) ->
	    <:expr<Bitmatch.construct_int64_ne_unsigned>>
	| (P.ConstantEndian NativeEndian, true) ->
	    <:expr<Bitmatch.construct_int64_ne_signed>>
	| (P.EndianExpr expr, false) ->
	    <:expr<Bitmatch.construct_int64_ee_unsigned $expr$>>
	| (P.EndianExpr expr, true) ->
	    <:expr<Bitmatch.construct_int64_ee_signed $expr$>>
      in

      let expr =
	match t, flen_is_const with
	(* Common case: int field, constant flen.
	 *
	 * Range checks are done inside the construction function
	 * because that's a lot simpler w.r.t. types.  It might
	 * be better to move them here. XXX
	 *)
	| P.Int, Some i when i > 0 && i <= 64 ->
	    let construct_fn = int_construct_const (i,endian,signed) in
	    exn_used := true;

	    <:expr<
	      $construct_fn$ $lid:buffer$ $fexpr$ $`int:i$ $lid:exn$
	    >>

	| P.Int, Some _ ->
	    fail "length of int field must be [1..64]"

	(* Int field, non-constant length.  We need to perform a runtime
	 * test to ensure the length is [1..64].
	 *
	 * Range checks are done inside the construction function
	 * because that's a lot simpler w.r.t. types.  It might
	 * be better to move them here. XXX
	 *)
	| P.Int, None ->
	    let construct_fn = int_construct (endian,signed) in
	    exn_used := true;

	    <:expr<
	      if $flen$ >= 1 && $flen$ <= 64 then
		$construct_fn$ $lid:buffer$ $fexpr$ $flen$ $lid:exn$
	      else
		raise (Bitmatch.Construct_failure
			 ("length of int field must be [1..64]",
			  $str:loc_fname$,
			  $int:loc_line$, $int:loc_char$))
	    >>

        (* String, constant length > 0, must be a multiple of 8. *)
	| P.String, Some i when i > 0 && i land 7 = 0 ->
	    let bs = gensym "bs" in
	    let j = i lsr 3 in
	    <:expr<
	      let $lid:bs$ = $fexpr$ in
	      if String.length $lid:bs$ = $`int:j$ then
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
	| P.String, Some (-1) ->
	    <:expr< Bitmatch.construct_string $lid:buffer$ $fexpr$ >>

	(* String, constant length = 0 is probably an error, and so is
	 * any other value.
	 *)
	| P.String, Some _ ->
	    fail "length of string must be > 0 and a multiple of 8, or the special value -1"

	(* String, non-constant length.
	 * We check at runtime that the length is > 0, a multiple of 8,
	 * and matches the declared length.
	 *)
	| P.String, None ->
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

        (* Bitstring, constant length >= 0. *)
	| P.Bitstring, Some i when i >= 0 ->
	    let bs = gensym "bs" in
	    <:expr<
	      let $lid:bs$ = $fexpr$ in
	      if Bitmatch.bitstring_length $lid:bs$ = $`int:i$ then
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
	| P.Bitstring, Some (-1) ->
	    <:expr< Bitmatch.construct_bitstring $lid:buffer$ $fexpr$ >>

	(* Bitstring, constant length < -1 is an error. *)
	| P.Bitstring, Some _ ->
	    fail "length of bitstring must be >= 0 or the special value -1"

	(* Bitstring, non-constant length.
	 * We check at runtime that the length is >= 0 and matches
	 * the declared length.
	 *)
	| P.Bitstring, None ->
	    let bslen = gensym "bslen" in
	    let bs = gensym "bs" in
	    <:expr<
	      let $lid:bslen$ = $flen$ in
	      if $lid:bslen$ >= 0 then (
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
  let fail = locfail _loc in

  let data = gensym "data" and off = gensym "off" and len = gensym "len" in
  let result = gensym "result" in

  (* This generates the field extraction code for each
   * field in a single case.  There must be enough remaining data
   * in the bitstring to satisfy the field.
   *
   * As we go through the fields, symbols 'data', 'off' and 'len'
   * track our position and remaining length in the bitstring.
   *
   * The whole thing is a lot of nested 'if' statements. Code
   * is generated from the inner-most (last) field outwards.
   *)
  let rec output_field_extraction inner = function
    | [] -> inner
    | field :: fields ->
	let fpatt = P.get_patt field in
	let flen = P.get_length field in
	let endian = P.get_endian field in
	let signed = P.get_signed field in
	let t = P.get_type field in
	let _loc = P.get_location field in
	let offset = P.get_offset field in

	(* Is flen (field len) an integer constant?  If so, what is it?
	 * This will be [Some i] if it's a constant or [None] if it's
	 * non-constant or we couldn't determine.
	 *)
	let flen_is_const = expr_is_constant flen in

	let int_extract_const = function
	    (* XXX The meaning of signed/unsigned breaks down at
	     * 31, 32, 63 and 64 bits.
	     *)
	  | (1, _, _) ->
	      <:expr<Bitmatch.extract_bit>>
	  | ((2|3|4|5|6|7|8), _, false) ->
	      <:expr<Bitmatch.extract_char_unsigned>>
	  | ((2|3|4|5|6|7|8), _, true) ->
	      <:expr<Bitmatch.extract_char_signed>>
	  | (i, P.ConstantEndian BigEndian, false) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_be_unsigned>>
	  | (i, P.ConstantEndian BigEndian, true) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_be_signed>>
	  | (i, P.ConstantEndian LittleEndian, false) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_le_unsigned>>
	  | (i, P.ConstantEndian LittleEndian, true) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_le_signed>>
	  | (i, P.ConstantEndian NativeEndian, false) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_ne_unsigned>>
	  | (i, P.ConstantEndian NativeEndian, true) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_ne_signed>>
	  | (i, P.EndianExpr expr, false) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_ee_unsigned $expr$>>
	  | (i, P.EndianExpr expr, true) when i <= 31 ->
	      <:expr<Bitmatch.extract_int_ee_signed $expr$>>
	  | (32, P.ConstantEndian BigEndian, false) ->
	      <:expr<Bitmatch.extract_int32_be_unsigned>>
	  | (32, P.ConstantEndian BigEndian, true) ->
	      <:expr<Bitmatch.extract_int32_be_signed>>
	  | (32, P.ConstantEndian LittleEndian, false) ->
	      <:expr<Bitmatch.extract_int32_le_unsigned>>
	  | (32, P.ConstantEndian LittleEndian, true) ->
	      <:expr<Bitmatch.extract_int32_le_signed>>
	  | (32, P.ConstantEndian NativeEndian, false) ->
	      <:expr<Bitmatch.extract_int32_ne_unsigned>>
	  | (32, P.ConstantEndian NativeEndian, true) ->
	      <:expr<Bitmatch.extract_int32_ne_signed>>
	  | (32, P.EndianExpr expr, false) ->
	      <:expr<Bitmatch.extract_int32_ee_unsigned $expr$>>
	  | (32, P.EndianExpr expr, true) ->
	      <:expr<Bitmatch.extract_int32_ee_signed $expr$>>
	  | (_, P.ConstantEndian BigEndian, false) ->
	      <:expr<Bitmatch.extract_int64_be_unsigned>>
	  | (_, P.ConstantEndian BigEndian, true) ->
	      <:expr<Bitmatch.extract_int64_be_signed>>
	  | (_, P.ConstantEndian LittleEndian, false) ->
	      <:expr<Bitmatch.extract_int64_le_unsigned>>
	  | (_, P.ConstantEndian LittleEndian, true) ->
	      <:expr<Bitmatch.extract_int64_le_signed>>
	  | (_, P.ConstantEndian NativeEndian, false) ->
	      <:expr<Bitmatch.extract_int64_ne_unsigned>>
	  | (_, P.ConstantEndian NativeEndian, true) ->
	      <:expr<Bitmatch.extract_int64_ne_signed>>
	  | (_, P.EndianExpr expr, false) ->
	      <:expr<Bitmatch.extract_int64_ee_unsigned $expr$>>
	  | (_, P.EndianExpr expr, true) ->
	      <:expr<Bitmatch.extract_int64_ee_signed $expr$>>
	in
	let int_extract = function
	  | (P.ConstantEndian BigEndian, false) ->
	      <:expr<Bitmatch.extract_int64_be_unsigned>>
	  | (P.ConstantEndian BigEndian, true) ->
	      <:expr<Bitmatch.extract_int64_be_signed>>
	  | (P.ConstantEndian LittleEndian, false) ->
	      <:expr<Bitmatch.extract_int64_le_unsigned>>
	  | (P.ConstantEndian LittleEndian, true) ->
	      <:expr<Bitmatch.extract_int64_le_signed>>
	  | (P.ConstantEndian NativeEndian, false) ->
	      <:expr<Bitmatch.extract_int64_ne_unsigned>>
	  | (P.ConstantEndian NativeEndian, true) ->
	      <:expr<Bitmatch.extract_int64_ne_signed>>
	  | (P.EndianExpr expr, false) ->
	      <:expr<Bitmatch.extract_int64_ee_unsigned $expr$>>
	  | (P.EndianExpr expr, true) ->
	      <:expr<Bitmatch.extract_int64_ee_signed $expr$>>
	in

	let expr =
	  match t, flen_is_const with
	  (* Common case: int field, constant flen *)
	  | P.Int, Some i when i > 0 && i <= 64 ->
	      let extract_fn = int_extract_const (i,endian,signed) in
	      let v = gensym "val" in
	      <:expr<
		if $lid:len$ >= $`int:i$ then (
		  let $lid:v$, $lid:off$, $lid:len$ =
		    $extract_fn$ $lid:data$ $lid:off$ $lid:len$ $`int:i$ in
		  match $lid:v$ with $fpatt$ when true -> $inner$ | _ -> ()
		)
	      >>

	  | P.Int, Some _ ->
	      fail "length of int field must be [1..64]"

	  (* Int field, non-const flen.  We have to test the range of
	   * the field at runtime.  If outside the range it's a no-match
	   * (not an error).
	   *)
	  | P.Int, None ->
	      let extract_fn = int_extract (endian,signed) in
	      let v = gensym "val" in
	      <:expr<
		if $flen$ >= 1 && $flen$ <= 64 && $flen$ <= $lid:len$ then (
		  let $lid:v$, $lid:off$, $lid:len$ =
		    $extract_fn$ $lid:data$ $lid:off$ $lid:len$ $flen$ in
		  match $lid:v$ with $fpatt$ when true -> $inner$ | _ -> ()
		)
	      >>

          (* String, constant flen > 0. *)
	  | P.String, Some i when i > 0 && i land 7 = 0 ->
	      let bs = gensym "bs" in
	      <:expr<
		if $lid:len$ >= $`int:i$ then (
		  let $lid:bs$, $lid:off$, $lid:len$ =
		    Bitmatch.extract_bitstring $lid:data$ $lid:off$ $lid:len$
		      $`int:i$ in
		  match Bitmatch.string_of_bitstring $lid:bs$ with
		  | $fpatt$ when true -> $inner$
		  | _ -> ()
		)
	      >>

          (* String, constant flen = -1, means consume all the
	   * rest of the input.
	   *)
	  | P.String, Some i when i = -1 ->
	      let bs = gensym "bs" in
	      <:expr<
		let $lid:bs$, $lid:off$, $lid:len$ =
		  Bitmatch.extract_remainder $lid:data$ $lid:off$ $lid:len$ in
		match Bitmatch.string_of_bitstring $lid:bs$ with
		| $fpatt$ when true -> $inner$
		| _ -> ()
	      >>

	  | P.String, Some _ ->
	      fail "length of string must be > 0 and a multiple of 8, or the special value -1"

	  (* String field, non-const flen.  We check the flen is > 0
	   * and a multiple of 8 (-1 is not allowed here), at runtime.
	   *)
	  | P.String, None ->
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
	  | P.Bitstring, Some i when i >= 0 ->
	      let ident =
		match fpatt with
		| <:patt< $lid:ident$ >> -> ident
		| <:patt< _ >> -> "_"
		| _ ->
		    fail "cannot compare a bitstring to a constant" in
	      <:expr<
		if $lid:len$ >= $`int:i$ then (
		  let $lid:ident$, $lid:off$, $lid:len$ =
		    Bitmatch.extract_bitstring $lid:data$ $lid:off$ $lid:len$
		      $`int:i$ in
		  $inner$
		)
	      >>

          (* Bitstring, constant flen = -1, means consume all the
	   * rest of the input.
	   *)
	  | P.Bitstring, Some i when i = -1 ->
	      let ident =
		match fpatt with
		| <:patt< $lid:ident$ >> -> ident
		| <:patt< _ >> -> "_"
		| _ ->
		    fail "cannot compare a bitstring to a constant" in
	      <:expr<
		let $lid:ident$, $lid:off$, $lid:len$ =
		  Bitmatch.extract_remainder $lid:data$ $lid:off$ $lid:len$ in
		  $inner$
	      >>

	  | P.Bitstring, Some _ ->
	      fail "length of bitstring must be >= 0 or the special value -1"

	  (* Bitstring field, non-const flen.  We check the flen is >= 0
	   * (-1 is not allowed here) at runtime.
	   *)
	  | P.Bitstring, None ->
	      let ident =
		match fpatt with
		| <:patt< $lid:ident$ >> -> ident
		| <:patt< _ >> -> "_"
		| _ ->
		    fail "cannot compare a bitstring to a constant" in
	      <:expr<
		if $flen$ >= 0 && $flen$ <= $lid:len$ then (
		  let $lid:ident$, $lid:off$, $lid:len$ =
		    Bitmatch.extract_bitstring $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  $inner$
		)
	      >>
	in

	(* Computed offset: only offsets forward are supported.
	 *
	 * We try hard to optimize this based on what we know.  Are
	 * we at a predictable offset now?  (Look at the outer 'fields'
	 * list and see if they all have constant field length starting
	 * at some constant offset).  Is this offset constant?
	 *
	 * Based on this we can do a lot of the computation at
	 * compile time, or defer it to runtime only if necessary.
	 *
	 * In all cases, the off and len fields get updated.
	 *)
	let expr =
	  match offset with
	  | None -> expr (* common case: there was no offset expression *)
	  | Some offset_expr ->
	      (* This will be [Some i] if offset is a constant expression
	       * or [None] if it's a non-constant.
	       *)
	      let requested_offset = expr_is_constant offset_expr in

	      (* This will be [Some i] if our current offset is known
	       * at compile time, or [None] if we can't determine it.
	       *)
	      let current_offset =
		let has_constant_offset field =
		  match P.get_offset field with
		  | None -> false
		  | Some expr ->
		      match expr_is_constant expr with
		      | None -> false
		      | Some i -> true
		in
		let get_constant_offset field =
		  match P.get_offset field with
		  | None -> assert false
		  | Some expr ->
		      match expr_is_constant expr with
		      | None -> assert false
		      | Some i -> i
		in

		let has_constant_len field =
		  match expr_is_constant (P.get_length field) with
		  | None -> false
		  | Some i when i > 0 -> true
		  | Some _ -> false
		in
		let get_constant_len field =
		  match expr_is_constant (P.get_length field) with
		  | None -> assert false
		  | Some i when i > 0 -> i
		  | Some _ -> assert false
		in

		let rec loop = function
		  (* first field has constant offset 0 *)
		  | [] -> Some 0
		  (* field with constant offset & length *)
		  | field :: _
		      when has_constant_offset field &&
			has_constant_len field ->
		      Some (get_constant_offset field + get_constant_len field)
		  (* field with no offset & constant length *)
		  | field :: fields
		      when P.get_offset field = None &&
			has_constant_len field ->
		      (match loop fields with
		       | None -> None
		       | Some offset -> Some (offset + get_constant_len field))
		  (* else, can't work out the offset *)
		  | _ -> None
		in
		loop fields in

              (* Look at the current offset and requested offset cases and
	       * determine what code to generate.
	       *)
	      match current_offset, requested_offset with
		(* This is the good case: both the current offset and
		 * the requested offset are constant, so we can remove
		 * almost all the runtime checks.
		 *)
	      | Some current_offset, Some requested_offset ->
		  let move = requested_offset - current_offset in
		  if move < 0 then
		    fail (sprintf "requested offset is less than the current offset (%d < %d)" requested_offset current_offset);
		  (* Add some code to move the offset and length by a
		   * constant amount, and a runtime test that len >= 0
		   * (XXX possibly the runtime test is unnecessary?)
		   *)
		  <:expr<
		    let $lid:off$ = $lid:off$ + $`int:move$ in
		    let $lid:len$ = $lid:len$ - $`int:move$ in
		    if $lid:len$ >= 0 then $expr$
		  >>
	      (* In any other case, we need to use runtime checks.
	       *
	       * XXX It's not clear if a backwards move detected at runtime
	       * is merely a match failure, or a runtime error.  At the
	       * moment it's just a match failure since bitmatch generally
	       * doesn't raise runtime errors.
	       *)
	      | _ ->
		  let move = gensym "move" in
		  <:expr<
		    let $lid:move$ = $offset_expr$ - $lid:off$ in
		    if $lid:move$ >= 0 then (
		      let $lid:off$ = $lid:off$ + $lid:move$ in
		      let $lid:len$ = $lid:len$ - $lid:move$ in
		      if $lid:len$ >= 0 then $expr$
		    )
		  >> in (* end of computed offset code *)

	(* Emit extra debugging code. *)
	let expr =
	  if not debug then expr else (
	    let field = P.string_of_pattern_field field in

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

(* Add a named pattern. *)
let add_named_pattern _loc name pattern =
  Hashtbl.add pattern_hash name pattern

(* Expand a named pattern from the pattern_hash. *)
let expand_named_pattern _loc name =
  try Hashtbl.find pattern_hash name
  with Not_found ->
    locfail _loc (sprintf "named pattern not found: %s" name)

(* Add named patterns from a file.  See the documentation on the
 * directory search path in bitmatch_persistent.mli
 *)
let load_patterns_from_file _loc filename =
  let chan =
    if Filename.is_relative filename && Filename.is_implicit filename then (
      (* Try current directory. *)
      try open_in filename
      with _ ->
	(* Try OCaml library directory. *)
	try open_in (Filename.concat Bitmatch_config.ocamllibdir filename)
	with exn -> Loc.raise _loc exn
    ) else (
      try open_in filename
      with exn -> Loc.raise _loc exn
    ) in
  let names = ref [] in
  (try
     let rec loop () =
       let name = P.named_from_channel chan in
       names := name :: !names
     in
     loop ()
   with End_of_file -> ()
  );
  close_in chan;
  let names = List.rev !names in
  List.iter (
    function
    | name, P.Pattern patt -> add_named_pattern _loc name patt
    | _, P.Constructor _ -> () (* just ignore these for now *)
  ) names

EXTEND Gram
  GLOBAL: expr str_item;

  (* Qualifiers are a list of identifiers ("string", "bigendian", etc.)
   * followed by an optional expression (used in certain cases).  Note
   * that we are careful not to declare any explicit reserved words.
   *)
  qualifiers: [
    [ LIST0
	[ q = LIDENT;
	  e = OPT [ "("; e = expr; ")" -> e ] -> (q, e) ]
	SEP "," ]
  ];

  (* Field used in the bitmatch operator (a pattern).  This can actually
   * return multiple fields, in the case where the 'field' is a named
   * persitent pattern.
   *)
  patt_field: [
    [ fpatt = patt; ":"; len = expr LEVEL "top";
      qs = OPT [ ":"; qs = qualifiers -> qs ] ->
	let field = P.create_pattern_field _loc in
	let field = P.set_patt field fpatt in
	let field = P.set_length field len in
	[parse_field _loc field qs]	(* Normal, single field. *)
    | ":"; name = LIDENT ->
	expand_named_pattern _loc name (* Named -> list of fields. *)
    ]
  ];

  (* Case inside bitmatch operator. *)
  patt_fields: [
    [ "{";
      fields = LIST0 patt_field SEP ";";
      "}" ->
	List.concat fields
    ]
  ];

  patt_case: [
    [ fields = patt_fields;
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
	let field = P.create_constructor_field _loc in
	let field = P.set_expr field fexpr in
	let field = P.set_length field len in
	parse_field _loc field qs
    ]
  ];

  constr_fields: [
    [ "{";
      fields = LIST0 constr_field SEP ";";
      "}" ->
	fields
    ]
  ];

  (* 'bitmatch' expressions. *)
  expr: LEVEL ";" [
    [ "bitmatch";
      bs = expr; "with"; OPT "|";
      cases = LIST1 patt_case SEP "|" ->
	output_bitmatch _loc bs cases
    ]

  (* Constructor. *)
  | [ "BITSTRING";
      fields = constr_fields ->
	output_constructor _loc fields
    ]
  ];

  (* Named persistent patterns.
   *
   * NB: Currently only allowed at the top level.  We can probably lift
   * this restriction later if necessary.  We only deal with patterns
   * at the moment, not constructors, but the infrastructure to do
   * constructors is in place.
   *)
  str_item: LEVEL "top" [
    [ "let"; "bitmatch";
      name = LIDENT; "="; fields = patt_fields ->
	add_named_pattern _loc name fields;
        (* The statement disappears, but we still need a str_item so ... *)
        <:str_item< >>
    | "open"; "bitmatch"; filename = STRING ->
	load_patterns_from_file _loc filename;
	<:str_item< >>
    ]
  ];

END
