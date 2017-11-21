(* Match random bits.
 * $Id$
 *)

open Printf

let rec range a b =
  if a <= b then
    a :: range (a+1) b
  else
    []

let () =
  Random.self_init ();

  for len = 0 to 999 do
    (* Create a random string of bits. *)
    let expected = List.map (fun _ -> Random.bool ()) (range 0 (len-1)) in

    let bits = Bitstring.Buffer.create () in
    List.iter (Bitstring.Buffer.add_bit bits) expected;
    let bits = Bitstring.Buffer.contents bits in

    (* Now read the bitstring in groups of 1, 2, 3 .. etc. bits.
     * In each case check the result against what we generated ('expected').
     *)
    let actual =
      let rec loop bits =
	bitmatch bits with
	| { b0 : 1; rest : -1 : bitstring } -> b0 :: loop rest
	| { _ } -> []
      in
      loop bits in
    if actual <> expected then
      failwith (sprintf "match bits: failed on 1 bit test, len = %d" len);

    let actual =
      let rec loop bits =
	bitmatch bits with
	| { b0 : 1; b1 : 1; rest : -1 : bitstring } -> b0 :: b1 :: loop rest
	| { b0 : 1; rest : -1 : bitstring } -> b0 :: loop rest
	| { _ } -> []
      in
      loop bits in
    if actual <> expected then
      failwith (sprintf "match bits: failed on 2 bit test, len = %d" len);

    let actual =
      let rec loop bits =
	bitmatch bits with
	| { b0 : 1; b1 : 1; b2 : 1;
	    rest : -1 : bitstring } -> b0 :: b1 :: b2 :: loop rest
	| { b0 : 1; rest : -1 : bitstring } -> b0 :: loop rest
	| { _ } -> []
      in
      loop bits in
    if actual <> expected then
      failwith (sprintf "match bits: failed on 3 bit test, len = %d" len);

    let actual =
      let rec loop bits =
	bitmatch bits with
	| { b0 : 1; b1 : 1; b2 : 1; b3 : 1;
	    rest : -1 : bitstring } -> b0 :: b1 :: b2 :: b3 :: loop rest
	| { b0 : 1; rest : -1 : bitstring } -> b0 :: loop rest
	| { _ } -> []
      in
      loop bits in
    if actual <> expected then
      failwith (sprintf "match bits: failed on 4 bit test, len = %d" len);

    let actual =
      let rec loop bits =
	bitmatch bits with
	| { b0 : 1; b1 : 1; b2 : 1; b3 : 1;
	    b4 : 1; b5 : 1; b6 : 1; b7 : 1;
	    b8 : 1;
	    rest : -1 : bitstring } ->
	    b0 :: b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: loop rest
	| { b0 : 1; rest : -1 : bitstring } -> b0 :: loop rest
	| { _ } -> []
      in
      loop bits in
    if actual <> expected then
      failwith (sprintf "match bits: failed on 9 bit test, len = %d" len);
  done
