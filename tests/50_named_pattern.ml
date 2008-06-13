(* Named pattern
 * $Id$
 *)

open Printf
open Bitmatch

(* A byte+length Pascal string. *)
let bitmatch pascal_string =
  { len : 8;
    str : len*8 : string }

let () =
  let bits = bitstring_of_string "\022Mary had a little lamb" in
  bitmatch bits with
  | { :pascal_string } ->
      () (*printf "it's a Pascal string, len = %d, string = %S\n" len str*)
  | { _ } ->
      eprintf "not matching error\n";
      exit 1
