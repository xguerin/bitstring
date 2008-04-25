(* Extract some simple integers.
 * $Id: 06_ints3.ml,v 1.2 2008-04-25 11:08:43 rjones Exp $
 *)

let bits = Bitmatch.make_bitstring 16 '\xcf' (* makes the string 0xcfcf *)

let () =
  bitmatch bits with
  | { n0 : 3; n1 : 3; n2 : 3; n3 : 3; n4 : 3; n5 : 1;
      rest : -1 : bitstring } ->
      assert (n0 = 0b110);
      assert (n1 = 0b011);
      assert (n2 = 0b111);
      assert (n3 = 0b100);
      assert (n4 = 0b111);
      assert (n5);

      let _, off, len = rest in
      assert (off = 16 && len = 0) (* no further data *)

  | { _ } ->
      failwith "error: did not match\n"
