(* Construct and match against random variable sized strings.
 * $Id: 20_varsize.ml,v 1.1 2008-04-01 17:05:37 rjones Exp $
 *)

open Printf

let nr_passes = 10000
let max_size = 8			(* max field size in bits *)

(* let () = Bitmatch.debug := true *)

(* Return a full 64 bits of randomness. *)
let rand64 () =
  let r0 = Int64.shift_left (Int64.of_int (Random.bits ())) 34 in (* 30 bits *)
  let r1 = Int64.shift_left (Int64.of_int (Random.bits ())) 4 in (* 30 bits *)
  let r2 = Int64.of_int (Random.int 16) in (* 4 bits *)
  Int64.logor (Int64.logor r0 r1) r2

(* Return unsigned mask of length bits, bits <= 64. *)
let mask64 bits =
  if bits < 63 then Int64.pred (Int64.shift_left 1L bits)
  else if bits = 63 then Int64.max_int
  else if bits = 64 then -1L
  else invalid_arg "mask64"

(* Return a random number between 0 and 2^bits-1 where bits <= 64. *)
let rand bits =
  let r = rand64 () in
  let m = mask64 bits in
  Int64.logand r m

(* Dump the state in case there is an error. *)
let dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits r0 r1 r2 r3 =
  eprintf "dumping state:\n";
  eprintf "  0: %3d - %016Lx - %016Lx\n" n0sz n0 r0;
  eprintf "  1: %3d - %016Lx - %016Lx\n" n1sz n1 r1;
  eprintf "  2: %3d - %016Lx - %016Lx\n" n2sz n2 r2;
  eprintf "  3: %3d - %016Lx - %016Lx\n" n3sz n3 r3;
  eprintf "bits (length = %d):\n" (Bitmatch.bitstring_length bits);
  Bitmatch.hexdump_bitstring stderr bits;
  eprintf "%!"

let () =
  Random.self_init ();

  for pass = 0 to nr_passes-1 do
    let n0sz = 1 + Random.int (max_size-1) in
    let n0   = rand n0sz in
    let n1sz = 1 + Random.int (max_size-1) in
    let n1   = rand n1sz in
    let n2sz = 1 + Random.int (max_size-1) in
    let n2   = rand n2sz in
    let n3sz = 1 + Random.int (max_size-1) in
    let n3   = rand n3sz in

    (* Construct the bitstring. *)
    let bits =
      try
	(BITSTRING
	  n0 : n0sz;
          n1 : n1sz;
	  n2 : n2sz;
	  n3 : n3sz)
      with
	Bitmatch.Construct_failure (msg, _, _, _) ->
	  eprintf "FAILED: Construct_failure %s\n%!" msg;
	  dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz
	    (Bitmatch.empty_bitstring) 0L 0L 0L 0L;
	  exit 2 in

    let r0, r1, r2, r3 =
      bitmatch bits with
      | r0 : n0sz; r1 : n1sz; r2 : n2sz; r3 : n3sz; rest : -1 : bitstring ->
	  let rest_len = Bitmatch.bitstring_length rest in
          if rest_len <> 0 then (
	    eprintf "FAILED: rest is not zero length (length = %d)\n%!"
	      rest_len;
	    dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits 0L 0L 0L 0L;
	    exit 2
	  );
          r0, r1, r2, r3
      | _ ->
	  eprintf "FAILED: bitmatch operator did not match\n%!";
	  dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits 0L 0L 0L 0L;
	  exit 2 in

    (*dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits r0 r1 r2 r3;*)

    if n0 <> r0 || n1 <> r1 || n2 <> r2 || n3 <> r3 then (
      eprintf "FAILED: numbers returned from match are different\n%!";
      dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits r0 r1 r2 r3;
      exit 2
    )
  done
