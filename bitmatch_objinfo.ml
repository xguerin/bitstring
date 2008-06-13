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

open Bitmatch
module P = Bitmatch_persistent

let () =
  if Array.length Sys.argv <= 1 then
    failwith "bitmatch_objinfo filename.bmpp";
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
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
    | name, P.Pattern patt ->
	printf "let bitmatch %s =\n%s\n"
	  name (P.string_of_pattern patt)
    | name, P.Constructor cons ->
	printf "let BITSTRING %s =\n%s\n"
	  name (P.string_of_constructor cons)
  ) names
