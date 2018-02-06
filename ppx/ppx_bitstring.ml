(*
 * Copyright (c) 2016 Xavier R. Guérin <copyright@applepine.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Migrate_parsetree
open Ast_405

open Ast_convenience_405
open Ast_mapper
open Asttypes
open Parsetree
open Lexing
open Printf

(*
 * Version management
 *)

let ocaml_version = Versions.ocaml_405

(* Type definition *)

module Entity = struct
  type t = {
    txt : string;
    exp : Parsetree.expression;
    pat : Parsetree.pattern
  }

  let mksym =
    let i = ref 1000 in
    fun name ->
      incr i; let i = !i in
      sprintf "__ppxbitstring_%s_%d" name i
  ;;

  let make ~loc v =
    let txt = mksym v in
    { txt; exp = evar ~loc txt; pat = pvar ~loc txt }
end

module Context = struct
  type t = {
    dat : Entity.t;
    off : Entity.t;
    len : Entity.t
  }

  let make ~loc =
    let dat = Entity.make ~loc "dat"
    and off = Entity.make ~loc "off"
    and len = Entity.make ~loc "len"
    in
    { dat; off; len }

  let next ~loc t =
    let off = Entity.make ~loc "off"
    and len = Entity.make ~loc "len"
    in
    { t with off; len }
end

module Type = struct
  type t =
    | Int
    | String
    | Bitstring
end

module Sign = struct
  type t =
    | Signed
    | Unsigned

  let to_string = function
    | Signed -> "signed"
    | Unsigned -> "unsigned"
end

module Endian = struct
  type t =
    | Little
    | Big
    | Native
    | Referred of Parsetree.expression

  let to_string = function
    | Little -> "le"
    | Big -> "be"
    | Native -> "ne"
    | Referred _ -> "ee"
end

module Qualifiers = struct
  type t = {
    value_type     : Type.t option;
    sign           : Sign.t option;
    endian         : Endian.t option;
    check          : Parsetree.expression option;
    bind           : Parsetree.expression option;
    map            : Parsetree.expression option;
    save_offset_to : Parsetree.expression option;
    offset         : Parsetree.expression option;
  }

  let empty = {
    value_type     = None;
    sign           = None;
    endian         = None;
    check          = None;
    bind           = None;
    map            = None;
    save_offset_to = None;
    offset         = None;
  }

  let default = {
    value_type     = Some Type.Int;
    sign           = Some Sign.Unsigned;
    endian         = Some Endian.Big;
    check          = None;
    bind           = None;
    map            = None;
    save_offset_to = None;
    offset         = None;
  }

  let set_value_type_default q =
    match q.value_type with
    | None  -> { q with value_type = Some Type.Int }
    | _     -> q
  ;;

  let set_sign_default q =
    match q.sign with
    | None  -> { q with sign = Some Sign.Unsigned }
    | _     -> q
  ;;

  let set_endian_default q =
    match q.endian with
    | None  -> { q with endian = Some Endian.Big }
    | _     -> q
  ;;

  let set_defaults v =
    v
    |> set_value_type_default
    |> set_sign_default
    |> set_endian_default
  ;;
end

module MatchField = struct
  type bitlen =
    (Parsetree.expression * int option)
  ;;

  type tuple = {
    pat : Parsetree.pattern;
    len : bitlen;
    qls : Qualifiers.t;
    opt : bool
  }

  type t =
    | Any of Parsetree.pattern
    | Tuple of tuple
  ;;
end

(* Exception *)

let location_exn ~loc msg =
  Location.Error (Location.error ~loc msg)
  |> raise
;;

(* Helper functions *)

let split_string ~on s =
  Str.split (Str.regexp on) s
;;

let option_bind opt f =
  match opt with
  | None   -> None
  | Some v -> f v
;;

let rec process_expr_loc ~loc expr =
  match expr with
  | { pexp_desc = Pexp_ident(ident) } ->
    let lident = Location.mkloc ident.txt loc in
    { expr with pexp_desc = Pexp_ident(lident); pexp_loc = loc }
  | { pexp_desc = Pexp_tuple(ops) } ->
    let fld = List.fold_left
        (fun acc exp -> acc @ [ process_expr_loc ~loc exp ])
        []
        ops
    in { expr with pexp_desc = Pexp_tuple(fld); pexp_loc = loc }
  | { pexp_desc = Pexp_construct(ident, ops) } ->
    let lident = Location.mkloc ident.txt loc in
    let lops = begin match ops with
      | Some o -> Some (process_expr_loc ~loc o)
      | None    -> None
    end in
    { expr with pexp_desc = Pexp_construct(lident, lops); pexp_loc = loc }
  | { pexp_desc = Pexp_apply(ident, ops) } ->
    let lident = process_expr_loc ~loc ident in
    let fld = List.fold_left
        (fun acc (lbl, exp) -> acc @ [ (lbl, (process_expr_loc ~loc exp)) ])
        []
        ops
    in { expr with pexp_desc = Pexp_apply(lident, fld); pexp_loc = loc }
  | { pexp_desc = Pexp_fun(ident, ops,
                           { ppat_desc = Ppat_var(pid); ppat_loc; ppat_attributes },
                           exp) } ->
    let lpid = Location.mkloc pid.txt loc in
    let lpat = { ppat_desc = Ppat_var lpid; ppat_loc = loc; ppat_attributes } in
    let lops = begin match ops with
      | Some o -> Some (process_expr_loc ~loc o)
      | None   -> None
    end in
    let lexp = process_expr_loc ~loc exp in
    { expr with pexp_desc = Pexp_fun(ident, lops, lpat, lexp); pexp_loc = loc }
  | _ ->
    { expr with pexp_loc = loc }
;;

let parse_expr expr =
  try
    Parse.expression Versions.ocaml_405 (Lexing.from_string expr.txt)
    |> process_expr_loc ~loc:expr.loc
  with
    _ -> location_exn ~loc:expr.loc ("Parse expression error: '" ^ expr.txt ^ "'")
;;

let rec process_pat_loc ~loc pat =
  match pat with
  | { ppat_desc = Ppat_var(ident); ppat_loc; ppat_attributes } ->
    let lident = Location.mkloc ident.txt loc in
    { ppat_desc = Ppat_var(lident); ppat_loc = loc; ppat_attributes }
  | _ ->
    { pat with ppat_loc = loc }
;;

let parse_pattern pat =
  try
    Parse.pattern Versions.ocaml_405 (Lexing.from_string pat.txt)
    |> process_pat_loc ~loc:pat.loc
  with
    _ -> location_exn ~loc:pat.loc ("Parse pattern error: '" ^ pat.txt ^ "'")
;;

(* Location parser and splitter *)

let find_loc_boundaries ~loc last rem =
  let open Location in
  let { loc_start; loc_end; loc_ghost } = loc in
  let xtr_lines = List.length rem in
  let xtr_char = List.fold_left (+) xtr_lines rem in
  let ne = { loc_start with
             pos_lnum = loc_start.pos_lnum + xtr_lines;
             pos_bol  = loc_start.pos_bol + xtr_char;
             pos_cnum = loc_start.pos_cnum + xtr_char + last
           }
  and ns = if xtr_lines = 0
    then { loc_start with
           pos_cnum = loc_start.pos_cnum + xtr_char + last + 1
         }
    else { loc_start with
           pos_lnum = loc_start.pos_lnum + xtr_lines;
           pos_bol  = loc_start.pos_bol + xtr_char;
           pos_cnum = loc_start.pos_cnum + xtr_char
         } in
  let tloc = { loc_start; loc_end = ne; loc_ghost } in
  let nloc = { loc_start = ns; loc_end; loc_ghost } in
  (tloc, nloc)
;;

let rec split_loc_rec ~loc = function
  | [] -> []
  | hd :: tl ->
    let line_list = split_string ~on:"\n" hd
                    |> List.rev
                    |> List.map String.length in
    begin
      match line_list with
      | [] -> []
      | last::rem ->
        let (tloc, nloc) = find_loc_boundaries ~loc last rem in
        [ tloc ] @ (split_loc_rec ~loc:nloc tl)
    end
;;

let split_loc ~loc lst =
  split_loc_rec ~loc lst
  |> List.map2 (fun e loc -> Location.mkloc (String.trim e) loc) lst
;;

(* Processing qualifiers *)

let check_map_functor sub =
  match sub with
  | [%expr (fun [%p? _] -> [%e? _])]  -> Some (sub)
  | _                                 -> None
;;

let process_qual state qual =
  let open Qualifiers in
  let loc = qual.pexp_loc in
  match qual with
  | [%expr int] ->
    begin match state.value_type with
      | Some v -> location_exn ~loc "Value type redefined"
      | None -> { state with value_type = Some Type.Int }
    end
  | [%expr string] ->
    begin match state.value_type with
      | Some v -> location_exn ~loc "Value type redefined"
      | None -> { state with value_type = Some Type.String }
    end
  | [%expr bitstring] ->
    begin match state.value_type with
      | Some v -> location_exn ~loc "Value type redefined"
      | None -> { state with value_type = Some Type.Bitstring }
    end
  | [%expr signed] ->
    begin match state.sign with
      | Some v -> location_exn ~loc "Signedness redefined"
      | None -> { state with sign = Some Sign.Signed }
    end
  | [%expr unsigned] ->
    begin match state.sign with
      | Some v -> location_exn ~loc "Signedness redefined"
      | None -> { state with sign = Some Sign.Unsigned }
    end
  | [%expr littleendian] ->
    begin match state.endian with
      | Some v -> location_exn ~loc "Endianness redefined"
      | None -> { state with endian = Some Endian.Little }
    end
  | [%expr bigendian] ->
    begin match state.endian with
      | Some v -> location_exn ~loc "Endianness redefined"
      | None -> { state with endian = Some Endian.Big }
    end
  | [%expr nativeendian] ->
    begin match state.endian with
      | Some v -> location_exn ~loc "Endianness redefined"
      | None -> { state with endian = Some Endian.Native }
    end
  | [%expr endian [%e? sub]] ->
    begin match state.endian with
      | Some v -> location_exn ~loc "Endianness redefined"
      | None -> { state with endian = Some (Endian.Referred sub) }
    end
  | [%expr bind [%e? sub]] ->
    begin match state.bind, state.map with
      | Some b, None   -> location_exn ~loc "Bind expression redefined"
      | None,   Some m -> location_exn ~loc "Map expression already defined"
      | Some b, Some m -> location_exn ~loc "Inconsistent internal state"
      | None,   None   -> { state with bind = Some sub }
    end
  | [%expr map [%e? sub]] ->
    begin match state.bind, state.map with
      | Some b, None   -> location_exn ~loc "Bind expression already defined"
      | None,   Some m -> location_exn ~loc "Map expression redefined"
      | Some b, Some m -> location_exn ~loc "Inconsistent internal state"
      | None,   None   -> begin
          match check_map_functor sub with
          | Some sub  -> { state with map = Some sub }
          | None      -> location_exn ~loc "Invalid map functor"
        end
    end
  | [%expr check [%e? sub]] ->
    begin match state.check with
      | Some v -> location_exn ~loc "Check expression redefined"
      | None -> { state with check = Some sub }
    end
  | [%expr save_offset_to [%e? sub]] ->
    begin match state.save_offset_to with
      | Some v -> location_exn ~loc "Save offset expression redefined"
      | None -> { state with save_offset_to = Some sub }
    end
  | [%expr offset [%e? sub]] ->
    begin match state.offset with
      | Some v -> location_exn ~loc "Offset expression redefined"
      | None -> { state with offset = Some sub }
    end
  | _ ->
    location_exn ~loc "Invalid qualifier"
;;

let parse_quals quals =
  let expr = parse_expr quals in
  let rec process_quals state = function
    | [] -> state
    | hd :: tl -> process_quals (process_qual state hd) tl
  in match expr with
  (* single named qualifiers *)
  | { pexp_desc = Pexp_ident (_) } ->
    process_qual Qualifiers.empty expr
  (* single functional qualifiers *)
  | { pexp_desc = Pexp_apply (_, _) } ->
    process_qual Qualifiers.empty expr
  (* multiple qualifiers *)
  | { pexp_desc = Pexp_tuple (e) } ->
    process_quals Qualifiers.empty e
  (* Unrecognized expression *)
  | expr ->
    location_exn ~loc:expr.pexp_loc "Invalid qualifiers list"
;;

(* Processing expression *)

let rec evaluate_expr = function
  | [%expr [%e? lhs] + [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l + r)
      | _ -> None
    end
  | [%expr [%e? lhs] - [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l - r)
      | _ -> None
    end
  | [%expr [%e? lhs] * [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l * r)
      | _ -> None
    end
  | [%expr [%e? lhs] / [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l / r)
      | _ -> None
    end
  | [%expr [%e? lhs] land [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l land r)
      | _ -> None
    end
  | [%expr [%e? lhs] lor [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l lor r)
      | _ -> None
    end
  | [%expr [%e? lhs] lxor [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l lxor r)
      | _ -> None
    end
  | [%expr [%e? lhs] lsr [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l lsr r)
      | _ -> None
    end
  | [%expr [%e? lhs] asr [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l asr r)
      | _ -> None
    end
  | [%expr [%e? lhs] mod [%e? rhs]] ->
    begin match evaluate_expr lhs, evaluate_expr rhs with
      | Some l, Some r -> Some (l mod r)
      | _ -> None
    end
  | { pexp_desc = Pexp_constant (const) } ->
    begin match const with
      | Pconst_integer(i, _) -> Some (int_of_string i)
      | _ -> None
    end
  | _ -> None
;;

(* Parsing fields *)

let parse_match_fields str =
  let open MatchField in
  split_string ~on:":" str.txt
  |> split_loc ~loc:str.loc
  |> function
  | [ { txt = "_" ; loc } as pat ] ->
    MatchField.Any (parse_pattern pat)
  | [ spat; slen ] ->
    let qls = Qualifiers.default
    and eln = parse_expr slen
    and pat = parse_pattern spat
    and opt = false in
    let len = (eln, evaluate_expr eln) in
    MatchField.Tuple { pat; len; qls; opt }
  | [ spat; slen; sqls ] ->
    let qls = Qualifiers.set_defaults (parse_quals sqls)
    and eln = parse_expr slen
    and pat = parse_pattern spat
    and opt = false in
    let len = (eln, evaluate_expr eln) in
    MatchField.Tuple { pat; len; qls; opt }
  | [ stmt ] ->
    let pat_str = stmt.txt in
    location_exn ~loc:stmt.loc ("Invalid statement: '" ^ pat_str ^ "'")
  | _ ->
    location_exn ~loc:str.loc "Invalid number of fields in statement"
;;

(*
 * Some operators like the subtype cast operator (:>) can throw off the parser.
 * The function below resolve these ambiguities on a case-by-case basis.
 *)
let stitch_ambiguous_operators lst =
  let fn e = function
    | [] -> [ e ]
    | hd :: tl when hd = "" || e == "" -> e :: hd :: tl
    | hd :: tl when Str.first_chars hd 1 = ">" -> (e ^ ":" ^ hd) :: tl
    | l -> e :: l
  in
  List.fold_right fn lst []

let parse_const_fields str =
  let open Qualifiers in
  split_string ~on:":" str.txt
  |> stitch_ambiguous_operators
  |> split_loc ~loc:str.loc
  |> function
  | [ vl; len ] ->
    (parse_expr vl, Some (parse_expr len), Some Qualifiers.default)
  | [ vl; len; quals ] ->
    let q = Qualifiers.set_defaults (parse_quals quals) in
    begin match q.bind, q.map, q.check, q.save_offset_to with
      | Some _, _, _, _ ->
        location_exn ~loc:str.loc "Bind meaningless in constructor"
      | _, Some _, _, _ ->
        location_exn ~loc:str.loc "Map meaningless in constructor"
      | _, _, Some _, _ ->
        location_exn ~loc:str.loc "Check meaningless in constructor"
      | _, _, _, Some _ ->
        location_exn ~loc:str.loc "Saving offset meaningless in constructor"
      | None, None, None, None ->
        (parse_expr vl, Some (parse_expr len), Some (q))
    end
  | [ stmt ] ->
    let pat_str = stmt.txt in
    location_exn ~loc:stmt.loc ("Invalid statement: '" ^ pat_str ^ "'")
  | _ ->
    location_exn ~loc:str.loc "Invalid number of fields in statement"
;;

(* Match generators *)

let check_field_len ~loc fld =
  let (l, v) = fld.MatchField.len
  in
  match v, fld.MatchField.qls.Qualifiers.value_type with
  | Some (n), Some (Type.String) ->
    if n < -1 || (n > 0 && (n mod 8) <> 0) then
      location_exn ~loc "Length of string must be > 0 and multiple of 8, or the special value -1"
    else Some n
  | Some (n), Some (Type.Bitstring) ->
    if n < -1 then location_exn ~loc "Length of bitstring must be >= 0 or the special value -1"
    else Some n
  | Some (n), Some (Type.Int) ->
    if n < 1 || n > 64 then location_exn ~loc "Length of int field must be [1..64]"
    else Some n
  | None, Some (_) -> None
  | _, None -> location_exn ~loc "No type to check"
;;

let get_inttype ~loc ~fastpath = function
  | v when v > 8  && v <= 16 -> if fastpath then "int16" else "int"
  | v when v > 16 && v <= 31 -> if fastpath then "int32" else "int"
  | v when v = 32 -> "int32"
  | v when v > 32 && v <= 64 -> "int64"
  | _ -> location_exn ~loc "Invalid integer size"

let gen_int_extractor_static ~loc nxt size sign endian =
  let edat = nxt.Context.dat.Entity.exp
  and eoff = nxt.Context.off.Entity.exp
  in
  let sn = Sign.to_string sign
  and ft = get_inttype ~loc ~fastpath:true size
  and en = Endian.to_string endian in
  let fp = sprintf "Bitstring.extract_fastpath_%s_%s_%s" ft en sn
  in
    [%expr
      [%e evar ~loc fp] [%e edat] ([%e eoff] lsr 3)]
      [@metaloc loc]
;;

let gen_int_extractor_dynamic ~loc nxt size sign endian =
  let edat = nxt.Context.dat.Entity.exp
  and eoff = nxt.Context.off.Entity.exp
  and elen = nxt.Context.len.Entity.exp
  in
  let sn = Sign.to_string sign
  and it = get_inttype ~loc ~fastpath:false size
  and en = Endian.to_string endian in
  let ex = sprintf "Bitstring.extract_%s_%s_%s" it en sn
  in
  [%expr [%e evar ~loc ex] [%e edat] [%e eoff] [%e elen] [%e int ~loc size]]
    [@metaloc loc]
;;

let gen_int_extractor ~loc nxt fld =
  let open Qualifiers in
  let (l, v) = fld.MatchField.len
  in
  let edat = nxt.Context.dat.Entity.exp
  and eoff = nxt.Context.off.Entity.exp
  and elen = nxt.Context.len.Entity.exp
  in
  match v, fld.MatchField.qls.sign, fld.MatchField.qls.endian with
    (* 1-bit type *)
    | Some (size), Some (_), Some (_) when size = 1 ->
      [%expr
        Bitstring.extract_bit [%e edat] [%e eoff] [%e elen] [%e l]]
        [@metaloc loc]
    (* 8-bit type *)
    | Some (size), Some (sign), Some (_) when size >= 2 && size <= 8 ->
      let ex = sprintf "Bitstring.extract_char_%s" (Sign.to_string sign)
      in
      [%expr
        [%e evar ~loc ex] [%e edat] [%e eoff] [%e elen] [%e int ~loc size]]
        [@metaloc loc]
    (* 16|32|64-bit type with referred endianness *)
    | Some (size), Some (sign), Some (Endian.Referred r) ->
      let ss = Sign.to_string sign
      and it = get_inttype ~loc ~fastpath:false size in
      let ex = sprintf "Bitstring.extract_%s_ee_%s" it ss
      in
      [%expr
        [%e evar ~loc ex] ([%e r]) [%e edat] [%e eoff] [%e elen] [%e int ~loc size]]
        [@metaloc loc]
    (* 16|32|64-bit type with immediate endianness *)
    | Some (size), Some (sign), Some (endian) ->
      if fld.MatchField.opt then
        gen_int_extractor_static ~loc nxt size sign endian
      else
        gen_int_extractor_dynamic ~loc nxt size sign endian
    (* Variable size *)
    | None, Some (sign), Some (Endian.Referred r) ->
      let ss = Sign.to_string sign in
      let ex = sprintf "Bitstring.extract_int64_ee_%s" ss in
      [%expr
        [%e evar ~loc ex] ([%e r]) [%e edat] [%e eoff] [%e elen] ([%e l])]
        [@metaloc loc]
    | None, Some (sign), Some (endian) ->
      let es = Endian.to_string endian and ss = Sign.to_string sign in
      let ex = sprintf "Bitstring.extract_int64_%s_%s" es ss in
      [%expr
        [%e evar ~loc ex] [%e edat] [%e eoff] [%e elen] ([%e l])]
        [@metaloc loc]
    (* Invalid type *)
    | _, _, _ ->
      location_exn ~loc "Invalid type"
;;

let gen_extractor ~loc nxt fld =
  let open Qualifiers in
  let (l, v) = fld.MatchField.len
  in
  let edat = nxt.Context.dat.Entity.exp
  and eoff = nxt.Context.off.Entity.exp
  and elen = nxt.Context.len.Entity.exp
  in
  match fld.MatchField.qls.value_type with
  | Some (Type.Bitstring) -> begin
      match v with
      | Some (-1) ->
        [%expr ([%e edat], [%e eoff], [%e elen])] [@metaloc loc]
      | Some (_) | None ->
        [%expr ([%e edat], [%e eoff], [%e l])] [@metaloc loc]
    end
  | Some (Type.String) ->
    [%expr
      (Bitstring.string_of_bitstring ([%e edat], [%e eoff], [%e l]))]
      [@metaloc loc]
  | Some (Type.Int) ->
    gen_int_extractor ~loc nxt fld
  | _ ->
    location_exn ~loc "Invalid type"
;;

let gen_value ~loc fld res beh =
  let open Qualifiers in
  match fld.MatchField.qls.bind, fld.MatchField.qls.map  with
  | Some b, None  ->
    [%expr let [%p fld.pat] = [%e b] in [%e beh]][@metaloc loc]
  | None, Some m  ->
    [%expr let [%p fld.pat] = [%e m] [%e res] in [%e beh]][@metaloc loc]
  | _, _ -> beh
;;

let rec gen_next ~loc cur nxt fld beh fields =
  let open Entity in
  let open Context in
  let (l, v) = fld.MatchField.len in
  match v with
  | Some (-1) ->
    [%expr
      let [%p nxt.off.pat] = [%e nxt.off.exp] + [%e nxt.len.exp]
      and [%p nxt.len.pat] = 0 in
      [%e (gen_fields ~loc cur nxt beh fields)]]
      [@metaloc loc]
  | Some (_) | None ->
    [%expr
      let [%p nxt.off.pat] = [%e nxt.off.exp] + [%e l]
      and [%p nxt.len.pat] = [%e nxt.len.exp] - [%e l] in
      [%e (gen_fields ~loc cur nxt beh fields)]]
      [@metaloc loc]

and gen_next_all ~loc cur nxt beh fields =
  let open Entity in
  let open Context in
  [%expr
    let [%p nxt.off.pat] = [%e nxt.off.exp] + [%e nxt.len.exp]
    and [%p nxt.len.pat] = 0 in
    [%e (gen_fields ~loc cur nxt beh fields)]]
    [@metaloc loc]

and gen_match_check ~loc = function
  | Some chk  -> chk
  | None      -> constr ~loc "true" []

and gen_match ~loc cur nxt fld beh fields =
  let open Entity in
  let open Context in
  let open Qualifiers in
  let value = Entity.make ~loc "val"
  and (l, _) = fld.MatchField.len
  in
  let mcheck = gen_match_check ~loc fld.MatchField.qls.check
  and mfields = gen_fields ~loc cur nxt beh fields
  and mres = gen_extractor ~loc nxt fld
  in
  let mwrap = gen_value ~loc fld value.exp mfields
  in
  let mcase = [%expr
    begin match [%e value.exp] with
      | [%p fld.MatchField.pat] when [%e mcheck] -> [%e mwrap]
      | _ -> ()
    end][@metaloc loc]
  in
  [%expr
    let [%p value.pat]   = [%e mres]
    and [%p nxt.off.pat] = [%e nxt.off.exp] + [%e l]
    and [%p nxt.len.pat] = [%e nxt.len.exp] - [%e l] in [%e mcase]]
    [@metaloc loc]

and gen_offset ~loc cur nxt fld beh =
  let open Context in
  let open Entity in
  let open Qualifiers in
  match fld.MatchField.qls.offset with
  | Some ({ pexp_loc } as off) ->
    [%expr
      let [%p nxt.off.pat] = [%e cur.off.exp] + [%e off] in [%e beh]]
      [@metaloc pexp_loc]
  | None -> beh

and gen_offset_saver ~loc cur nxt fld beh =
  let open Context in
  let open Entity in
  let open Qualifiers in
  match fld.MatchField.qls.save_offset_to with
  | Some { pexp_desc = Pexp_ident ({ txt; loc = eloc }) } ->
    let ptxt = pvar ~loc:eloc (Longident.last txt) in
    [%expr
      let [%p ptxt] = [%e nxt.off.exp] - [%e cur.off.exp] in [%e beh]]
      [@metaloc eloc]
  | Some _ | None -> beh

and gen_unbound_string ~loc cur nxt fld beh fields =
  let open Entity in
  let open Context in
  let p = fld.MatchField.pat
  in
  match p with
  | { ppat_desc = Ppat_var(_) } ->
    [%expr
      let [%p p] = [%e (gen_extractor ~loc nxt fld)] in
      [%e (gen_next_all ~loc cur nxt beh fields)]]
      [@metaloc loc]
  | [%pat? _ ] ->
    [%expr
      [%e (gen_next_all ~loc cur nxt beh fields)]]
      [@metaloc loc]
  | _ ->
    location_exn ~loc "Unbound string or bitstring can only be assigned to a variable or skipped"

and gen_bound_bitstring ~loc cur nxt fld beh fields =
  let open Entity in
  let open Context in
  let p = fld.MatchField.pat
  and (l, _) = fld.MatchField.len
  in
  match p with
  | { ppat_desc = Ppat_var(_) } ->
    [%expr
      if Pervasives.(>=) [%e nxt.len.exp] [%e l] then
        let [%p p] = [%e (gen_extractor ~loc nxt fld)] in
        [%e (gen_next ~loc cur nxt fld beh fields)]
      else ()]
      [@metaloc loc]
  | [%pat? _ ] ->
    [%expr
      if Pervasives.(>=) [%e nxt.len.exp] [%e l] then
        [%e (gen_next ~loc cur nxt fld beh fields)]
      else ()]
      [@metaloc loc]
  | _ ->
    location_exn ~loc "Bound bitstring can only be assigned to variables or skipped"

and gen_bound_string ~loc cur nxt fld beh fields =
  let open Entity in
  let open Context in
  let (l, _) = fld.MatchField.len
  in
  [%expr
    if Pervasives.(>=) [%e nxt.len.exp] [%e l] then
      [%e (gen_match ~loc cur nxt fld beh fields)]
    else ()]
    [@metaloc loc]

and gen_bound_int_with_size ~loc cur nxt fld beh fields =
  let open Entity in
  let open Context in
  let (l, _) = fld.MatchField.len
  in
  [%expr
    if Pervasives.(>=) [%e nxt.len.exp] [%e l] then
      [%e (gen_match ~loc cur nxt fld beh fields)]
    else ()]
    [@metaloc loc]

and gen_bound_int ~loc cur nxt fld beh fields =
  let open Entity in
  let open Context in
  let (l, _) = fld.MatchField.len
  in
  [%expr
    if Pervasives.(>=) [%e l]           1  &&
       Pervasives.(<=) [%e l]           64 &&
       Pervasives.(>=) [%e nxt.len.exp] [%e l] then
      [%e (gen_match ~loc cur nxt fld beh fields)]
    else ()]
    [@metaloc loc]

and gen_fields_with_quals_by_type ~loc cur nxt fld beh fields =
  let open Qualifiers in
  match check_field_len ~loc fld, fld.MatchField.qls.value_type with
  | Some (-1), Some (Type.Bitstring | Type.String) ->
    gen_unbound_string ~loc cur nxt fld beh fields
  | (Some (_) | None), Some (Type.Bitstring) ->
    gen_bound_bitstring ~loc cur nxt fld beh fields
  | (Some (_) | None), Some (Type.String) ->
    gen_bound_string ~loc cur nxt fld beh fields
  | Some (s), Some (Type.Int) ->
    if s >= 1 && s <= 64 then
      gen_bound_int_with_size ~loc cur nxt fld beh fields
    else
      location_exn ~loc "Invalid bit length for type Integer"
  | None, Some (Type.Int) ->
    gen_bound_int ~loc cur nxt fld beh fields
  | _, _ ->
    location_exn ~loc "No type to generate"

and gen_fields_with_quals ~loc cur nxt fld beh fields =
  gen_fields_with_quals_by_type ~loc cur nxt fld beh fields
  |> gen_offset_saver ~loc cur nxt fld
  |> gen_offset ~loc cur nxt fld

and gen_fields ~loc cur nxt beh fields =
  let open Qualifiers in
  let (exp, alias) = beh
  in
  match fields with
  | [] ->
    begin match alias with
      | None -> exp
      | Some a -> [%expr
          let [%p pvar ~loc a] = ([%e cur.dat.exp], [%e cur.off.exp], ([%e cur.len.exp] - [%e nxt.len.exp]))
          in
          [%e exp]
        ][@metaloc loc]
    end
  | MatchField.Any (_) :: tl ->
    begin match alias with
      | None -> exp
      | Some a -> [%expr
          let [%p pvar ~loc a] = ([%e cur.dat.exp], [%e cur.off.exp], [%e cur.len.exp])
          in
          [%e exp]
        ][@metaloc loc]
    end
  | MatchField.Tuple (fld) :: tl -> gen_fields_with_quals ~loc cur nxt fld beh tl
;;

let is_field_size_open_ended = function
  | (_, Some (-1))  -> true
  | _               -> false

let check_for_open_endedness fields =
  let check init fld =
    let p = fld.MatchField.pat
    and l = fld.MatchField.len in
    let oe = is_field_size_open_ended l in
    if init || (oe && init) then
      location_exn ~loc:p.ppat_loc "Pattern is already open-ended"
    else oe
  in
  let inspect init = function
    | MatchField.Any (_) -> init && false
    | MatchField.Tuple fld -> check init fld
  in
  let rec scan init = function
    | [] -> ()
    | hd :: tl -> scan (inspect init hd) tl
  in
  scan false fields; fields
;;

let mark_optimized_fastpath fields =
  let open Qualifiers in
  let open MatchField
  in
  let check_field off tuple =
    match tuple with
    | { pat; len = (l, Some (v)); qls = { value_type = Some (Type.Int) }; _ } ->
      if (off land 7) = 0 && (v = 16 || v = 32 || v = 64) then
        (Some (off + v), MatchField.Tuple { tuple with opt = true })
      else
        (None, MatchField.Tuple tuple)
    | _ ->
      (None, MatchField.Tuple tuple)
  in
  let check_offset_and_field offset fld =
    match offset, fld with
    | Some (off), MatchField.Tuple (tuple) -> check_field off tuple
    | _, _ -> (None, fld)
  in
  let rec scan offset result = function
    | [] -> result
    | hd :: tl ->
      let (noff, nfld) = check_offset_and_field offset hd in
      scan noff (result @ [ nfld ]) tl
  in
  scan (Some 0) [] fields
;;

let gen_case_constant ~loc cur nxt res case value alias =
  let open Entity in
  let beh = [%expr
    [%e res.exp] := Some ([%e case.pc_rhs]);
    raise Exit][@metaloc loc]
  in
  let beh =
    match case.pc_guard with
    | None -> beh
    | Some cond -> [%expr if [%e cond] then [%e beh] else ()][@metaloc loc]
  in
  split_string ~on:";" value
  |> split_loc ~loc
  |> List.map parse_match_fields
  |> check_for_open_endedness
  |> mark_optimized_fastpath
  |> gen_fields ~loc cur nxt (beh, alias)

let gen_case cur nxt res case =
  let open Entity in
  let loc = case.pc_lhs.ppat_loc in
  match case.pc_lhs.ppat_desc with
  | Ppat_constant (Pconst_string (value, _)) ->
    gen_case_constant ~loc cur nxt res case value None
  | Ppat_alias ({ ppat_desc = Ppat_constant (Pconst_string (value, _)) }, { txt = a }) ->
    gen_case_constant ~loc cur nxt res case value (Some a)
  | _ ->
    location_exn ~loc "Wrong pattern type"
;;

let rec gen_cases_sequence ~loc = function
  | []        -> location_exn ~loc "Empty case list"
  | [hd]      -> hd
  | hd :: tl  -> [%expr [%e hd]; [%e gen_cases_sequence ~loc tl]][@metaloc loc]
;;

let gen_cases ~loc ident cases =
  let open Entity in
  let open Context in
  let cur = Context.make ~loc
  and res = Entity.make ~loc "res"
  in
  let nxt = Context.next ~loc cur
  and tupl = [%pat? ([%p cur.dat.pat], [%p cur.off.pat], [%p cur.len.pat])][@metaloc loc]
  and fnam = str ~loc loc.Location.loc_start.pos_fname
  and lpos = int ~loc loc.Location.loc_start.pos_lnum
  and cpos = int ~loc (loc.Location.loc_start.pos_cnum - loc.Location.loc_start.pos_bol)
  in
  List.fold_left
    (fun acc case -> acc @ [ gen_case cur nxt res case ])
    []
    cases
  |> gen_cases_sequence ~loc
  |> fun seq ->
  [%expr
    let [%p tupl]        = [%e ident] in
    let [%p nxt.off.pat] = [%e cur.off.exp]
    and [%p nxt.len.pat] = [%e cur.len.exp]
    and [%p res.pat]     = ref None
    in
    (try [%e seq]; with | Exit -> ());
    match ![%e res.exp] with
    | Some x -> x
    | None -> raise (Match_failure ([%e fnam], [%e lpos], [%e cpos]))]
    [@metaloc loc]
;;

let gen_function ~loc cases =
  let open Entity in
  let cas = Entity.make ~loc "case" in
  [%expr
    (fun [%p cas.pat] -> [%e (gen_cases ~loc cas.exp cases)])]
    [@metaloc loc]

(* Constructor generators *)

let gen_constructor_exn ~loc =
  let open Location in
  [%expr Bitstring.Construct_failure (
      [%e str ~loc "Bad field value"],
        [%e str ~loc loc.loc_start.pos_fname],
        [%e int ~loc loc.loc_start.pos_lnum],
        [%e int ~loc loc.loc_start.pos_cnum])]
    [@metaloc loc]
;;

let gen_constructor_bitstring ~loc sym (l, _, _ ) =
  [%expr
    Bitstring.construct_bitstring [%e sym.Entity.exp] [%e l]]
    [@metaloc loc]
;;

let gen_constructor_string ~loc sym (l, _, _) =
  [%expr
    Bitstring.construct_string [%e sym.Entity.exp] [%e l]]
    [@metaloc loc]
;;

let get_1_bit_constr_value ~loc (l, _, _) =
  match (evaluate_expr l) with
    | Some (1)        -> [%expr true][@metaloc loc]
    | Some (0)        -> [%expr false][@metaloc loc]
    | Some (_) | None -> l
;;

let gen_constructor_int ~loc sym fld =
  let open Qualifiers in
  let (l, s, q) = fld in
  let eexc = gen_constructor_exn ~loc
  and esym = sym.Entity.exp in
  let (fnc, vl, sz) = match (evaluate_expr s), q.sign, q.endian with
    (* 1-bit type *)
    | Some (size), Some (_), Some (_) when size = 1 ->
      (evar ~loc "Bitstring.construct_bit", get_1_bit_constr_value ~loc fld, [%expr 1])
    (* 8-bit type *)
    | Some (size), Some (sign), Some (_) when size >= 2 && size <= 8 ->
      let sn = Sign.to_string sign in
      let ex = sprintf "Bitstring.construct_char_%s" sn in
      (evar ~loc ex, l, int ~loc size)
    (* 16|32|64-bit type *)
    | Some (size), Some (sign), Some (Endian.Referred r) ->
      let ss = Sign.to_string sign
      and it = get_inttype ~loc ~fastpath:false size in
      let ex = sprintf "Bitstring.construct_%s_ee_%s" it ss in
      ([%expr [%e evar ~loc ex] [%e r]], l, s)
    | Some (size), Some (sign), Some (endian) ->
      let tp = get_inttype ~loc ~fastpath:false size
      and en = Endian.to_string endian
      and sn = Sign.to_string sign in
      let ex = sprintf "Bitstring.construct_%s_%s_%s" tp en sn in
      (evar ~loc ex, l, int ~loc size)
    (* Variable size types *)
    | None, Some (sign), Some (Endian.Referred r) ->
      let ss = Sign.to_string sign in
      let ex = sprintf "Bitstring.construct_int64_ee_%s" ss in
      ([%expr [%e evar ~loc ex] [%e r]], l, s)
    | None, Some (sign), Some (endian) ->
      let en = Endian.to_string endian
      and sn = Sign.to_string sign in
      let ex = sprintf "Bitstring.construct_int64_%s_%s" en sn in
      (evar ~loc ex, l, s)
    (* Invalid type *)
    | _, _, _ ->
      location_exn ~loc "Invalid type"
  in
  [%expr
    [%e fnc] [%e esym] [%e vl] [%e sz] [%e eexc]]
    [@metaloc loc]
;;

let gen_constructor_complete ~loc sym fld =
  let (_, _, q) = fld in
  match q.Qualifiers.value_type with
  | Some (Type.Bitstring) -> gen_constructor_bitstring ~loc sym fld
  | Some (Type.String)    -> gen_constructor_string ~loc sym fld
  | Some (Type.Int)       -> gen_constructor_int ~loc sym fld
  | _                     -> location_exn ~loc "Invalid type"
;;

let gen_constructor ~loc sym = function
  | (f, Some (s), Some (q)) -> gen_constructor_complete ~loc sym (f, s, q)
  | _ -> location_exn ~loc "Invalid field format"
;;

let gen_assignment_size_of_sized_field ~loc (f, s, q) =
  match (evaluate_expr s), option_bind q (fun q -> q.Qualifiers.value_type) with
  (* Deal with String type *)
  | Some (-1), Some (Type.String) -> [%expr (String.length [%e f] * 8)]
  | Some (v),  Some (Type.String) when v > 0 && (v mod 8) = 0 -> s
  | Some (_),  Some (Type.String) ->
      location_exn ~loc "Length of string must be > 0 and multiple of 8, or the special value -1"
  (* Deal with Bitstring type *)
  | Some (-1), Some (Type.Bitstring) -> [%expr (Bitstring.bitstring_length [%e f])]
  | Some (v),  Some (Type.Bitstring) when v > 0 -> s
  | Some (_),  Some (Type.Bitstring) ->
      location_exn ~loc "Length of bitstring must be >= 0 or the special value -1"
  (* Deal with other types *)
  | Some (v), _ when v > 0 -> s
  | Some (v), _ ->
      location_exn ~loc "Negative or null field size in constructor"
  (* Unknown field size, arbitrary expression *)
  | None, _ -> s
;;

let gen_assignment_size_of_field ~loc = function
  | (_, None, _) -> [%expr 0]
  | (f, Some (s), q) -> gen_assignment_size_of_sized_field ~loc (f, s, q)
;;

let rec gen_assignment_size ~loc = function
  | [] -> [%expr 0]
  | field :: tl ->
     let this = gen_assignment_size_of_field ~loc field in
     let next = gen_assignment_size ~loc tl in
     [%expr [%e this] + ([%e next])][@metaloc loc]
;;

let gen_assignment_behavior ~loc sym fields =
  let size = gen_assignment_size ~loc fields in
  let res = sym.Entity.exp in
  let rep = [%expr Bitstring.Buffer.contents [%e res]][@metaloc loc] in
  let len = match (evaluate_expr size) with
    | Some (v)  -> int v
    | None      -> size
  in
  let post =
    [%expr
      let _res = [%e rep] in
      if Pervasives.(=) (Bitstring.bitstring_length _res) [%e len]
      then _res else raise Exit]
      [@metaloc loc]
  in
  let seq = List.fold_right
      (fun fld acc -> [%expr [%e (gen_constructor ~loc sym fld)]; [%e acc]])
      fields
      post
  in
  [%expr
    let [%p sym.Entity.pat] = Bitstring.Buffer.create () in
    [%e seq]]
    [@metaloc loc]
;;

let parse_assignment_behavior ~loc sym value =
  split_string ~on:";" value
  |> split_loc ~loc
  |> List.map (fun flds -> parse_const_fields flds)
  |> gen_assignment_behavior ~loc sym
;;

let gen_constructor_expr ~loc value =
  let open Entity in
  let sym = Entity.make ~loc "constructor" in
  let beh = parse_assignment_behavior ~loc sym value in
  [%expr let [%p sym.pat] = fun () -> [%e beh] in [%e sym.exp] ()]
;;

let transform_single_let ~loc ast expr =
  match ast.pvb_pat.ppat_desc, ast.pvb_expr.pexp_desc with
  | Parsetree.Ppat_var (s), Pexp_constant (Pconst_string (value, _)) ->
    let pat = pvar ~loc s.txt in
    let constructor_expr = gen_constructor_expr loc value in
    [%expr let [%p pat] = [%e constructor_expr] in [%e expr]]
  | _ -> location_exn ~loc "Invalid pattern type"
;;

(*
 * Rewriter. See:
 * https://github.com/let-def/ocaml-migrate-parsetree/blob/master/MANUAL.md#new-registration-interface
 *)

let extension expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (value, (_ : string option))) ->
    gen_constructor_expr loc value
  | Pexp_let (Nonrecursive, bindings, expr) ->
    List.fold_right
      (fun binding expr -> transform_single_let ~loc binding expr)
      bindings
      expr
  | Pexp_match (ident, cases) ->
    gen_cases ~loc ident cases
  | Pexp_function (cases) ->
    gen_function ~loc cases
  | _ ->
    location_exn ~loc
      "'bitstring' can only be used with 'let', 'match', and as '[%bitstring]'"

let expression mapper = function
  | [%expr [%bitstring [%e? e0]]] -> mapper.expr mapper (extension e0)
  | expr -> Ast_mapper.default_mapper.expr mapper expr

let structure_item_mapper mapper = function
  | [%stri [%%bitstring let [%p? var] = [%e? e0]]] ->
    [%stri let [%p mapper.pat mapper var] = [%e mapper.expr mapper (extension e0)]]
  | stri -> Ast_mapper.default_mapper.structure_item mapper stri

let rewriter config cookies = {
  Ast_mapper.default_mapper with
  expr = expression;
  structure_item = structure_item_mapper;
}

let () =
  Driver.register ~name:"ppx_bitstring" ~args:[] Versions.ocaml_405 rewriter
;;
