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

open OUnit2
open Bitstring

(*
 * Test of the map() qualifier
 *)

let map_test context =
  let source = [%bitstring {| 1 : 16 ; 2 : 16 |}] in
  match%bitstring source with
  | {| value0 : 16 : map (fun v -> v + 1)
     ; value1 : 16 : map (fun v -> Some v)
    |} ->
    assert_equal value0 2;
    begin match value1 with
      | Some v -> assert_equal v 2
      | _      -> assert_bool "Invalid map result" false
    end
  | {| _ |} -> assert_bool "Invalid pattern" false

(*
 * Test of the save_offset_to() qualifier
 *)

let save_offset_test context =
  let source = [%bitstring {| 1 : 3 ; 2 : 7; 5 : 4; "abc" : -1 : string |}] in
  match%bitstring source with
  | {| _      :  3 : save_offset_to (off0)
     ; _      :  7 : save_offset_to (off1)
     ; _      :  4 : save_offset_to (off2)
     ; "abc"  : 24 : save_offset_to (off3), string
    |} ->
    assert_equal off0 0;
    assert_equal off1 3;
    assert_equal off2 10;
    assert_equal off3 14
  | {| _ |} -> assert_bool "Invalid pattern" false

(*
 * Test suite definition
 *)

let suite = "BitstringQualifierTest" >::: [
    "map"             >:: map_test;
    "save_offset_to"  >:: save_offset_test
  ]

let () = run_test_tt_main suite
