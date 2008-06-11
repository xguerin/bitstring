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
  let cpp_args = ref [] in
  let cpp_arg2 name value =
    cpp_args := (name ^ value) :: !cpp_args
  in

  let argspec = Arg.align [
    "--debug", Arg.Set debug,
      " Debug messages";
    "--version", Arg.Unit version,
      " Display version and exit";
    "-save-temps", Arg.Set save_temps,
      " Save temporary files";
    "-I", Arg.String (cpp_arg2 "-I"),
      "dir Specify extra include directory for cpp";
    "-D", Arg.String (cpp_arg2 "-D"),
      "name=value Define value in cpp";
    "-U", Arg.String (cpp_arg2 "-U"),
      "name Undefine value in cpp";
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
  let cpp_args = List.rev !cpp_args in

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
    sprintf "cpp %s -include bitmatch-import-prefix.h %s > %s"
      (String.concat " " (List.map Filename.quote cpp_args))
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

  (* Output constants. *)
  List.iter (
    fun (vname, vinit, loc) ->
      printf "let %s = 0x%LX\n" vname vinit
  ) constants;

  (* Output structures. *)
  List.iter (
    fun (tname, ttype, loc) ->
      (* Uncomment the next line if you want to really print the
       * complete CIL structure of the type (for debugging etc.).
       * The ASTs printed here are usually quite large.
       *)
      (*Errormsg.log "%a: %s %a\n" d_loc loc tname d_plaintype ttype;*)

      (* Match on the type of this structure, and from it generate
       * a single parsing function.
       *)
      match ttype with
	(* struct or union *)
      | TComp ({ cdefined = true; cname = cname }, _) ->
	  printf "let %s_of_bitstring bits =\n" tname;
	  printf "  bitmatch bits with\n";
	  printf "  | {\n";
	  (*output_struct [] NoOffset None ttype;*)
	  printf "    } ->\n";
	  printf "    Some (...)\n";
	  printf "  | { _ } -> None\n\n"

      (* An undefined struct or union -- means one which was only ever
       * defined with 'struct foo;'.  This is an error.
       *)
      | TComp ({ cdefined = false; cname = cname }, _) ->
	  Errormsg.error
	    "%a: struct or union has no definition: %s" d_loc loc cname

      (* Types which are not allowed, eg. void, int, arrays. *)
      | TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _
      | TNamed _ | TBuiltin_va_list _ ->
	  Errormsg.error
	    "%a: not a struct or union: %a" d_loc loc d_type ttype

      (* Types which we might implement in the future.
       * For enum we should probably split out enums separately
       * from structs above, since enums are more like constants.
       *)
      | TEnum ({ ename = ename }, _) ->
	  Errormsg.unimp "%a: %a" d_loc loc d_type ttype
(*
      let rec to_fields names offset endian = function
	  (* Some types contain attributes to indicate their
	   * endianness.  See many examples from <linux/types.h>.
	   *)
	| (TNamed ({ tname = tname;
		     ttype = TNamed (_, attrs) },
		   _) as t)
	    when hasAttribute "bitwise" attrs ->
	    let endian =
	      if String.starts_with tname "__le" then
		Some Bitmatch.LittleEndian
	      else if String.starts_with tname "__be" then
		Some Bitmatch.BigEndian
	      else (
		Errormsg.warn "%a: unknown bitwise attribute typename: %s\n"
		  d_loc loc tname;
		endian
	      ) in
	    to_fields names offset endian (unrollType t)

        (* See into named types. *)
	| (TNamed _ as t) ->
	    to_fields names offset endian (unrollType t)

	(* struct or union *)
	| TComp ({ cdefined = true; cfields = cfields }, _) ->
	    let cfields =
	      List.map (
		fun ({ fname = fname; ftype = ftype } as finfo) ->
		  let offset = Field (finfo, offset) in
		  let names = fname :: names in
		  to_fields names offset endian ftype
	      ) cfields in
	    List.flatten cfields

	(* int array with constant length *)
	| TArray (basetype, (Some _ as len), _)
	    when isIntegralType basetype ->
	    let len = lenOfArray len in
	    let bitsoffset, totalwidth = bitsOffset ttype offset in
	    let bitswidth = totalwidth / len (* of the element *) in
	    (*if debug then (
	      let name = String.concat "." (List.rev names) in
	      Errormsg.log "%s: int array: %d, %d, len %d\n"
		name bitsoffset bitswidth len
	    );*)
	    let basetype = unrollType basetype in
	    let ikind =
	      match basetype with
	      | TInt (ikind, _) -> ikind
	      | t ->
		  Errormsg.unimp "%a: unhandled type: %a" d_loc loc d_type t;
		  IInt in
	    let field =
	      to_int_field "" bitsoffset bitswidth ikind endian in
	    let fname = String.concat "_" (List.rev names) in
	    let byteoffset = bitsoffset lsr 3 in
	    let bytetotalwidth = totalwidth lsr 3 in
	    printf "--> array %s: byteoffset=%d bytetotalwidth=%d len=%d\n"
	      fname byteoffset bytetotalwidth len (* field *);
	    []

	(* basic integer type *)
	| TInt (ikind, _) ->
	    let bitsoffset, bitswidth = bitsOffset ttype offset in
	    (*if debug then (
	      let name = String.concat "." (List.rev names) in
	      Errormsg.log "%s: int: %d, %d\n" name bitsoffset bitswidth
	    );*)
	    let fname = String.concat "_" (List.rev names) in
	    let field =
	      to_int_field fname bitsoffset bitswidth ikind endian in
	    [field]

	(* a pointer - in this mapping we assume this is an address
	 * (endianness and wordsize come from function parameters),
	 * in other words we DON'T try to follow pointers, we just
	 * note that they are there.
	 *)
	| TPtr _ ->
	    let bitsoffset, bitswidth = bitsOffset ttype offset in
	    let fname = String.concat "_" (List.rev names) in
	    printf "--> pointer %s: bitsoffset=%d bitswidth=%d\n"
	      fname bitsoffset bitswidth;
	    []

	| t ->
	    Errormsg.unimp "to_fields: %a: unhandled type: %a"
	      d_loc loc d_type t;
	    []

      and to_int_field fname bitsoffset bitswidth ikind endian =
	let byteoffset = bitsoffset lsr 3 in
	let bytewidth = bitswidth lsr 3 in
	let signed = isSigned ikind in

	if bitsoffset land 7 = 0 && bitswidth land 7 = 0 then (
	  (* Not a bitfield. *)
	  match bitswidth with
	  | 8 ->
	      printf "--> byte %s: byteoffset=%d bytewidth=%d signed=%b\n"
		fname byteoffset bytewidth signed
	  | 16 ->
	      printf "--> short %s: byteoffset=%d bytewidth=%d signed=%b endian=%s\n"
		fname byteoffset bytewidth signed (Option.map_default Bitmatch.string_of_endian "None" endian)
	  | 32 ->
	      printf "--> int %s: byteoffset=%d bytewidth=%d signed=%b endian=%s\n"
		fname byteoffset bytewidth signed (Option.map_default Bitmatch.string_of_endian "None" endian)
	  | 64 ->
	      printf "--> long %s: byteoffset=%d bytewidth=%d signed=%b endian=%s\n"
		fname byteoffset bytewidth signed (Option.map_default Bitmatch.string_of_endian "None" endian)
	  | _ ->
	      Errormsg.unimp "%s: unhandled integer width: %d bits"
		fname bitswidth
	) else (
	  (* It's a bitfield if either the offset or width isn't
	   * byte-aligned.
	   *)
	  let bitsoffset = bitsoffset land 7 in
	  printf "--> bitfield %s: byteoffset=%d bytewidth=%d signed=%b endian=%s bitsoffset=%d bitswidth=%d\n"
	    fname byteoffset bytewidth
	    signed (Option.map_default Bitmatch.string_of_endian "None" endian) bitsoffset bitswidth
	)
      in
*)
  ) structs;

  if !Errormsg.hadErrors then exit 1;

  exit 0
