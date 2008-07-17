(* Test check() and bind().
 * $Id$
 *)

open Printf
open Bitmatch

let bits = (BITSTRING { 101 : 16; 202 : 16 })

let () =
  bitmatch bits with
  | { i : 16 : check (i > 100), bind (i*4);
      j : 16 : check (j > 200) } ->
      if i <> 404 || j <> 202 then
	failwith (sprintf "70_check_and_bind: failed: %d %d" i j)
  | { _ } ->
      failwith "70_check_and_bind: match failed"
