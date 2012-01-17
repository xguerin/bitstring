(* Test functions which construct and extract fixed-length ints
 * of various sizes.
 * $Id$
 *)

open Printf

open Bitstring

let () =
  for i = 0 to 129 do
    let zeroes = zeroes_bitstring i in
    let bits = (
      BITSTRING {
	zeroes : i : bitstring;
	true : 1;
	2 : 2 : littleendian;
	2 : 2 : bigendian;
	2 : 2 : nativeendian;
	3 : 3 : littleendian;
	3 : 3 : bigendian;
	3 : 3 : nativeendian;
	0x5a : 8 : littleendian;
	0x5a : 8 : bigendian;
	0x5a : 8 : nativeendian;
	0xa5a5 : 16 : littleendian;
	0xa5a5 : 16 : bigendian;
	0xa5a5 : 16 : nativeendian;
	0xeeddcc : 24 : littleendian;
	0xeeddcc : 24 : bigendian;
	0xeeddcc : 24 : nativeendian;
	0x48888888 : 31 : littleendian;
	0x48888888 : 31 : bigendian;
	0x48888888 : 31 : nativeendian;
	0xaabbccdd_l : 32 : littleendian;
	0xaabbccdd_l : 32 : bigendian;
	0xaabbccdd_l : 32 : nativeendian;
	0xaabbccddeeff_L : 48 : littleendian;
	0xaabbccddeeff_L : 48 : bigendian;
	0xaabbccddeeff_L : 48 : nativeendian;
	0x0011aabbccddeeff_L : 64 : littleendian;
	0x0011aabbccddeeff_L : 64 : bigendian;
	0x0011aabbccddeeff_L : 64 : nativeendian
      }
    ) in
    bitmatch bits with
    | { _ : i : bitstring;
	a : 1;
	b0 : 2 : littleendian;
	b1 : 2 : bigendian;
	b2 : 2 : nativeendian;
	c0 : 3 : littleendian;
	c1 : 3 : bigendian;
	c2 : 3 : nativeendian;
	d0 : 8 : littleendian;
	d1 : 8 : bigendian;
	d2 : 8 : nativeendian;
	e0 : 16 : littleendian;
	e1 : 16 : bigendian;
	e2 : 16 : nativeendian;
	f0 : 24 : littleendian;
	f1 : 24 : bigendian;
	f2 : 24 : nativeendian;
	g0 : 31 : littleendian;
	g1 : 31 : bigendian;
	g2 : 31 : nativeendian;
	h0 : 32 : littleendian;
	h1 : 32 : bigendian;
	h2 : 32 : nativeendian;
	j0 : 48 : littleendian;
	j1 : 48 : bigendian;
	j2 : 48 : nativeendian;
	k0 : 64 : littleendian;
	k1 : 64 : bigendian;
	k2 : 64 : nativeendian
      } ->
	if a <> true
	  || b0 <> 2
	  || b1 <> 2
	  || b2 <> 2
	  || c0 <> 3
	  || c1 <> 3
	  || c2 <> 3
	  || d0 <> 0x5a
	  || d1 <> 0x5a
	  || d2 <> 0x5a
	  || e0 <> 0xa5a5
	  || e1 <> 0xa5a5
	  || e2 <> 0xa5a5
	  || f0 <> 0xeeddcc
	  || f1 <> 0xeeddcc
	  || f2 <> 0xeeddcc
	  || g0 <> 0x48888888
	  || g1 <> 0x48888888
	  || g2 <> 0x48888888
	  || h0 <> 0xaabbccdd_l
	  || h1 <> 0xaabbccdd_l
	  || h2 <> 0xaabbccdd_l
	  || j0 <> 0xaabbccddeeff_L
	  || j1 <> 0xaabbccddeeff_L
	  || j2 <> 0xaabbccddeeff_L
	  || k0 <> 0x0011aabbccddeeff_L
	  || k1 <> 0x0011aabbccddeeff_L
	  || k2 <> 0x0011aabbccddeeff_L
	then (
	    eprintf "15_extract_int: match failed %b %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %ld %ld %ld %Ld %Ld %Ld %Ld %Ld %Ld\n"
	      a b0 b1 b2 c0 c1 c2 d0 d1 d2 e0 e1 e2 f0 f1 f2 g0 g1 g2 h0 h1 h2 j0 j1 j2 k0 k1 k2;
	    exit 1
	  )
    | { _ } ->
	failwith "15_extract_int"
  done
