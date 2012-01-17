(* Test concat and the bit get functions.
 * $Id$
 *)

let () =
  for i = 0 to 33 do
    for j = 0 to 33 do
      for k = 0 to 33 do
	let bits =
	  Bitstring.concat [
	    Bitstring.ones_bitstring i;
	    Bitstring.zeroes_bitstring j;
	    Bitstring.ones_bitstring k
	  ] in
	assert (Bitstring.bitstring_length bits = i+j+k);
	for n = 0 to i-1 do
	  assert (Bitstring.is_set bits n)
	done;
	for n = i to i+j-1 do
	  assert (Bitstring.is_clear bits n)
	done;
	for n = i+j to i+j+k-1 do
	  assert (Bitstring.is_set bits n)
	done
      done
    done
  done
