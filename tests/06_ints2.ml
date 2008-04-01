(* Extract some simple integers.
 * $Id: 06_ints2.ml,v 1.1 2008-04-01 08:56:43 rjones Exp $
 *)

let bits = Bitmatch.make_bitstring 16 '\xcf' (* makes the string 0xcfcf *)

let () =
  bitmatch bits with
  | n0 : 2; n1 : 2; n2 : 2; n3 : 2; n4 : 2; n5 : 2; n6 : 2; n7 : 2;
    rest : -1 : bitstring ->
      assert (n0 = 0x3); (* 0xc *)
      assert (n1 = 0x0);
      assert (n2 = 0x3); (* 0xf *)
      assert (n3 = 0x3);
      assert (n4 = 0x3); (* 0xc *)
      assert (n5 = 0x0);
      assert (n6 = 0x3); (* 0xf *)
      assert (n7 = 0x3);

      let _, off, len = rest in
      assert (off = 16 && len = 0) (* no further data *)

  | _ ->
      failwith "error: did not match\n"
