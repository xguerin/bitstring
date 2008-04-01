(* Test a simple constructor.
 * $Id: 10_constr1.ml,v 1.1 2008-04-01 17:05:37 rjones Exp $
 *)

let bits = BITSTRING 0xc : 4; 0xf : 4; 0xc : 4; 0xf : 4 ;;

assert (bits = Bitmatch.make_bitstring 16 '\xcf') ;;

let () =
  bitmatch bits with
  | n0 : 4; n1 : 4; n2 : 4; n3 : 4;
    rest : -1 : bitstring ->
      assert (n0 = 0xc);
      assert (n1 = 0xf);
      assert (n2 = 0xc);
      assert (n3 = 0xf);

      let _, off, len = rest in
      assert (off = 16 && len = 0) (* no further data *)

  | _ ->
      failwith "error: did not match\n"
