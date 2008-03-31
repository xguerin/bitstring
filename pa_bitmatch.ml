(* Bitmatch syntax extension.
 * $Id: pa_bitmatch.ml,v 1.1 2008-03-31 22:52:17 rjones Exp $
 *)

open Printf

open Camlp4.PreCast
open Syntax
open Ast

type m = Fields of f list		(* field ; field -> ... *)
       | Bind of string option		(* _ -> ... *)
and f = {
  ident : string;			(* field name *)
  flen : expr;				(* length in bits, may be non-const *)
  endian : endian;			(* endianness *)
  signed : bool;			(* true if signed, false if unsigned *)
  t : t;				(* type *)
}
and endian = BigEndian | LittleEndian | NativeEndian
and t = Int | Bitstring

(* Generate a fresh, unique symbol each time called. *)
let gensym =
  let i = ref 1000 in
  fun name ->
    incr i; let i = !i in
    sprintf "__pabitmatch_%s_%d" name i

(* Deal with the qualifiers which appear for a field. *)
let output_field _loc name flen qs =
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

  (* If type is set to bitstring then endianness and signedness
   * qualifiers are meaningless and must not be set.
   *)
  if t = Some Bitstring && (endian <> None || signed <> None) then
    Loc.raise _loc (
      Failure "bitstring type and endian or signed qualifiers cannot be mixed"
    );

  (* Default endianness, signedness, type. *)
  let endian = match endian with None -> BigEndian | Some e -> e in
  let signed = match signed with None -> false | Some s -> s in
  let t = match t with None -> Int | Some t -> t in

  {
    ident = name;
    flen = flen;
    endian = endian;
    signed = signed;
    t = t;
  }

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
    | {ident=ident; flen=flen; endian=endian; signed=signed; t=t} :: fields ->
	(* If length an integer constant?  If so, what is it?  This
	 * is very simple-minded and only detects simple constants.
	 *)
	let flen_is_const =
	  match flen with
	  | <:expr< $int:i$ >> -> Some (int_of_string i)
	  | _ -> None in

	let name_of_int_extract_const = function
	  | (1, _, _) -> "extract_bit"
	  | ((2|3|4|5|6|7), _, false) -> "extract_char_unsigned"
	  | ((2|3|4|5|6|7), _, true) -> "extract_char_signed"
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
	      <:expr<
		if $lid:len$ >= $flen$ then (
		  let $lid:ident$, $lid:off$, $lid:len$ =
		    Bitmatch.$lid:extract_func$ $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  $inner$
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
	      <:expr<
		if $flen$ >= 1 && $flen$ <= 64 && $flen$ >= $lid:len$ then (
		  let $lid:ident$, $lid:off$, $lid:len$ =
		    Bitmatch.$lid:extract_func$ $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  $inner$
		)
	      >>

          (* Bitstring, constant flen >= 0. *)
	  | Bitstring, Some i when i >= 0 ->
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
	      <:expr<
		if $flen$ >= 0 && $lid:len$ >= $flen$ then (
		  let $lid:ident$, $lid:off$, $lid:len$ =
		    Bitmatch.extract_bitstring $lid:data$ $lid:off$ $lid:len$
		      $flen$ in
		  $inner$
		)
	      >>
	in

	output_field_extraction expr fields
  in

  (* Convert each case in the match. *)
  let cases = List.map (
    function
    (* field : len ; field : len when .. -> ..*)
    | (Fields fields, Some whenclause, code) ->
	let inner =
	  <:expr<
	    if $whenclause$ then (
	      $lid:result$ := Some ($code$);
	      raise Exit
            )
	  >> in
	output_field_extraction inner (List.rev fields)

    (* field : len ; field : len -> ... *)
    | (Fields fields, None, code) ->
	let inner =
	  <:expr<
	    $lid:result$ := Some ($code$);
	    raise Exit
	  >> in
	output_field_extraction inner (List.rev fields)

    (* _ as name when ... -> ... *)
    | (Bind (Some name), Some whenclause, code) ->
	<:expr<
	  let $lid:name$ = ($lid:data$, $lid:off$, $lid:len$) in
	  if $whenclause$ then (
	    $lid:result$ := Some ($code$);
	    raise Exit
	  )
	>>

    (* _ as name -> ... *)
    | (Bind (Some name), None, code) ->
	<:expr<
	  let $lid:name$ = ($lid:data$, $lid:off$, $lid:len$) in
	  $lid:result$ := Some ($code$);
	  raise Exit
	>>

    (* _ when ... -> ... *)
    | (Bind None, Some whenclause, code) ->
	<:expr<
	  if $whenclause$ then (
	    $lid:result$ := Some ($code$);
	    raise Exit
	  )
	>>

    (* _ -> ... *)
    | (Bind None, None, code) ->
	<:expr<
	  $lid:result$ := Some ($code$);
	  raise Exit
	>>

  ) cases in

  let cases =
    List.fold_right (fun case base -> <:expr< $case$ ; $base$ >>)
      cases <:expr< () >> in

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

  field: [
    [ name = LIDENT; ":"; len = expr LEVEL "top";
      qs = OPT [ ":"; qs = qualifiers -> qs ] ->
	output_field _loc name len qs
    ]
  ];

  match_case: [
    [ fields = LIST0 field SEP ";";
      w = OPT [ "when"; e = expr -> e ]; "->";
      code = expr ->
	(Fields fields, w, code)
    ]
  | [ "_";
      bind = OPT [ "as"; name = LIDENT -> name ];
      w = OPT [ "when"; e = expr -> e ]; "->";
      code = expr ->
	(Bind bind, w, code)
    ]
  ];

  expr: LEVEL ";" [
    [ "bitmatch"; bs = expr; "with"; OPT "|";
      cases = LIST1 match_case SEP "|" ->
	output_bitmatch _loc bs cases
    ]
  ];

END
