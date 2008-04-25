(* Extract some simple integers.
 * $Id: 06_ints1.ml,v 1.2 2008-04-25 11:08:43 rjones Exp $
 *)

let bits = Bitmatch.make_bitstring 16 '\xcf' (* makes the string 0xcfcf *)

let () =
  bitmatch bits with
  | { n0 : 4; n1 : 4; n2 : 4; n3 : 4;
      rest : -1 : bitstring } ->
      assert (n0 = 0xc);
      assert (n1 = 0xf);
      assert (n2 = 0xc);
      assert (n3 = 0xf);

      let _, off, len = rest in
      assert (off = 16 && len = 0) (* no further data *)

  | { _ } ->
      failwith "error: did not match\n"
