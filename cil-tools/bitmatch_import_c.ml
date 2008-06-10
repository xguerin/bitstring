(* Import a C header file.
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
open ExtList
open ExtString
open Cil

let () =
  (* Parse command line arguments. *)
  let debug = ref false in
  let save_temps = ref false in
  let version () =
    printf "bitmatch-import-c %s" Bitmatch.version;
    exit 1
  in

  let argspec = Arg.align [
    "--debug", Arg.Set debug,
      " Debug messages";
    "-save-temps", Arg.Set save_temps,
      " Save temporary files";
    "--version", Arg.Unit version,
      " Display version and exit";
  ] in

  let input_file = ref None in
  let anon_fun str =
    match !input_file with
    | None -> input_file := Some str
    | Some _ ->
	eprintf "bitmatch-import-c: only give a single input file\n";
	exit 1
  in
  let usage_msg = "\

bitmatch-import-c: Import C structures and constants and
  generate bitmatching functions from them.  Please see the
  manual page bitmatch-import-c(1) for more information.

OPTIONS" in

  Arg.parse argspec anon_fun usage_msg;

  let debug = !debug in
  let save_temps = !save_temps in
  let input_file =
    match !input_file with
    | Some f -> f
    | None ->
	eprintf "bitmatch-import-c: no input file specified\n";
	exit 1 in

  (* Grab the file and pass it to the preprocessor, and then read the
   * C code into memory using CIL.
   *)
  msvcMode := false;
  Cil.initCIL ();

  (* XXX Unavoidable tmp exploit here.  Fix? *)
  let tmp, delete_tmp =
    if not save_temps then (
      let tmp = Filename.temp_file (Filename.temp_dir_name) ".i" in
      tmp, fun () -> try Unix.unlink tmp with Unix.Unix_error _ -> ()
    ) else (
      let tmp = Filename.chop_extension input_file ^ ".i" in
      tmp, fun () -> (* -save-temps, so do nothing *) ()
    ) in

  let cmd =
    sprintf "cpp -include bitmatch-import-prefix.h %s > %s"
      (Filename.quote input_file) (Filename.quote tmp) in
  if debug then prerr_endline cmd;
  if Sys.command cmd <> 0 then (
    eprintf "%s: command failed\n" cmd;
    delete_tmp ();
    exit 1
  );

  (* Why does Frontc.parse return a continuation ...? *)
  let file = (Frontc.parse tmp) () in
  delete_tmp ();

  (* Find out which structures, #defines, etc. are to be imported.
   * (cf. the macros in bitmatch-import-prefix.h)
   *)
  let constants =
    List.filter_map (
      function
      | GVar ({vname = vname; vtype = vtype},
	      { init = Some (SingleInit vinit) },
	      loc)
	  when String.starts_with vname "__bitmatch_constant_" ->
	  let vname = String.sub vname 20 (String.length vname - 20) in

	  (* Do constant folding on the initializer and then calculate
	   * its compile-time value.
	   *)
	  let vinit =
	    match isInteger (constFold true vinit) with
	    | Some i -> i
	    | None ->
		Errormsg.error
		  "%a: non-constant initializer: %a" d_loc loc d_exp vinit;
		-1L in

	  Some (vname, vinit, loc)
      | _ -> None
    ) file.globals in
  let structs =
    List.filter_map (
      function
      | GType ({tname = tname; ttype = ttype}, loc)
	  when String.starts_with tname "__bitmatch_import_" ->
	  let tname = String.sub tname 18 (String.length tname - 18) in
	  Some (tname, ttype, loc)
      | _ -> None
    ) file.globals in

  if !Errormsg.hadErrors then exit 1;

  (* If debugging, print out the imports. *)
  if debug then (
    List.iter (
      fun (vname, vinit, loc) ->
	Errormsg.log "%a: import %s as constant 0x%LX\n" d_loc loc vname vinit;
    ) constants;
    List.iter (
      fun (tname, ttype, loc) ->
	Errormsg.log "%a: import %s as %a\n" d_loc loc tname d_type ttype;
    ) structs;
  );

