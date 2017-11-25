(* Test the Bitstring.Buffer module and string_of_bitstring in
 * nasty non-aligned corner cases.
 * $Id$
 *)

open Printf

let () =
  Random.self_init ();

  let str1 = Bytes.of_string "012345678" in

  for offset = 0 to 65 do
    for len = 1 to 65 do
      let expected =
	let strlen = (len+7) lsr 3 in
	let expected = Bytes.create strlen in
	for i = 0 to strlen-1 do
	  Bytes.set expected i (Char.chr (Random.int 256))
	done;
	let last = Char.code (Bytes.get expected (strlen-1)) in
	let last = last land (0xff lsl (8 - (len land 7))) in
	Bytes.set expected (strlen-1) (Char.chr last);
	expected in

      (* Create a random bitstring:
       * +-------------+-------------------------------------------+
       * | (random)    | bits that we check (expected)             |
       * +-------------+-------------------------------------------+
       * 0           offset                                    offset+len
       *                <---------------- len bits --------------->
       *)
      let bits =
	let bits = Bitstring.Buffer.create () in
	Bitstring.Buffer.add_bits bits str1 offset;
	Bitstring.Buffer.add_bits bits expected len;
	Bitstring.Buffer.contents bits in

      (* Create a sub bitstring corresponding to what we want to check. *)
      let subbits =
	let bits, bitoffset, bitlen = bits in
	(bits, bitoffset+offset, bitlen-offset) in

      assert (Bitstring.bitstring_length subbits = len);

      (* Now try to read out the substring using string_of_bitstring. *)
      let actual = Bitstring.string_of_bitstring subbits in
      if Bytes.of_string actual <> expected then (
	eprintf "MISMATCH between actual and expected, offset=%d, len=%d\n"
	  offset len;
	eprintf "EXPECTED string:\n";
	for i = 0 to Bytes.length expected-1 do
	  eprintf " %02x" (Char.code (Bytes.get expected i))
	done;
	eprintf "\nACTUAL string:\n";
	for i = 0 to String.length actual-1 do
	  eprintf " %02x" (Char.code actual.[i])
	done;
	eprintf "\nBITS:\n";
	Bitstring.hexdump_bitstring stderr bits;
	eprintf "SUBBITS:\n";
	Bitstring.hexdump_bitstring stderr subbits;
	exit 1
      );
    done
  done
