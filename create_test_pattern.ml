(* Create persistent pattern.
 * $Id$
 *)

open Bitmatch_persistent

open Camlp4.PreCast
open Syntax
open Ast

let () =
  let _loc = Loc.ghost in

  let len_field = create_pattern_field _loc in
  let len_field = set_length_int len_field 8 in
  let len_field = set_lident_patt len_field "len" in

  let str_field = create_pattern_field _loc in
  let str_field = set_length str_field <:expr< len*8 >> in
  let str_field = set_lident_patt str_field "str" in
  let str_field = set_type_string str_field in

  let named_pattern = "pascal_string", Pattern [len_field; str_field] in

  let chan = open_out Sys.argv.(1) in
  named_to_channel chan named_pattern;
  close_out chan
