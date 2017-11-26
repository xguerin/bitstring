open OUnit2
open Printf

(*
 * Helper functions
 *)

let rec range a b =
  if a <= b then
    a :: range (a+1) b
  else
    []

(*
 * Just check that the extension and library load without error.
 *)

let load_test _ =
  let _ = Bitstring.extract_bit in ()

(*
 * Just check that we can run some functions from the library.
 *)

let run_test _ =
  let bits = Bitstring.create_bitstring 16 in
  ignore (Bitstring.string_of_bitstring bits)

(*
 * Match random bits.
 *)

let match_random_bits_test _ =
  Random.self_init ();

  for len = 0 to 999 do
    (*
     * Create a random string of bits.
     *)
    let expected = List.map (fun _ -> Random.bool ()) (range 0 (len-1)) in

    let bits = Bitstring.Buffer.create () in
    List.iter (Bitstring.Buffer.add_bit bits) expected;
    let bits = Bitstring.Buffer.contents bits in
    (*
     * Now read the bitstring in groups of 1, 2, 3 .. etc. bits.  In each case
     * check the result against what we generated ('expected').
     *)
    let actual =
      let rec loop bits =
        match%bitstring bits with
        | {| b0 : 1; rest : -1 : bitstring |} -> b0 :: loop rest
        | {| _ |} -> []
      in
      loop bits in
    assert_equal actual expected;
    (*
     *)
    let actual =
      let rec loop bits =
        match%bitstring bits with
        | {| b0 : 1; b1 : 1; rest : -1 : bitstring |} -> b0 :: b1 :: loop rest
        | {| b0 : 1; rest : -1 : bitstring |} -> b0 :: loop rest
        | {| _ |} -> []
      in
      loop bits in
    assert_equal actual expected;
    (*
     *)
    let actual =
      let rec loop bits =
        match%bitstring bits with
        | {| b0 : 1; b1 : 1; b2 : 1;
             rest : -1 : bitstring |} -> b0 :: b1 :: b2 :: loop rest
        | {| b0 : 1; rest : -1 : bitstring |} -> b0 :: loop rest
        | {| _ |} -> []
      in
      loop bits in
    assert_equal actual expected;
    (*
     *)
    let actual =
      let rec loop bits =
        match%bitstring bits with
        | {| b0 : 1; b1 : 1; b2 : 1; b3 : 1;
             rest : -1 : bitstring |} -> b0 :: b1 :: b2 :: b3 :: loop rest
        | {| b0 : 1; rest : -1 : bitstring |} -> b0 :: loop rest
        | {| _ |} -> []
      in
      loop bits in
    assert_equal actual expected;
    (*
     *)
    let actual =
      let rec loop bits =
        match%bitstring bits with
        | {| b0 : 1; b1 : 1; b2 : 1; b3 : 1;
             b4 : 1; b5 : 1; b6 : 1; b7 : 1;
             b8 : 1;
             rest : -1 : bitstring |} ->
          b0 :: b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: loop rest
        | {| b0 : 1; rest : -1 : bitstring |} -> b0 :: loop rest
        | {| _ |} -> []
      in
      loop bits in
    assert_equal actual expected;
  done

(*
 * Match random bits with integers.
 *)

let match_random_bits_with_int_test _ =
  Random.self_init ();

  for len = 1 to 99 do
    for bitlen = 1 to 63 do
      (*
       * Create a random string of ints.
       *)
      let expected =
        List.map (fun _ ->
            Random.int64 (Int64.sub (Int64.shift_left 1L bitlen) 1L))
          (range 0 (len-1)) in

      let bits = Bitstring.Buffer.create () in
      List.iter (fun i ->
          Bitstring.construct_int64_be_unsigned bits i bitlen
            (Failure "constructing string"))
        expected;
      let bits = Bitstring.Buffer.contents bits in
      (*
       * Now read the bitstring as integers.
       * In each case check the result against what we generated ('expected').
       *)
      let actual =
        let rec loop bits =
          match%bitstring bits with
          | {| i : bitlen; rest : -1 : bitstring |}
            when Bitstring.bitstring_length rest = 0 -> [i]
          | {| i : bitlen; rest : -1 : bitstring |} -> i :: loop rest
          | {| _ |} ->
            failwith (sprintf "loop failed with len = %d, bitlen = %d"
                        len bitlen)
        in
        loop bits in
      assert_equal actual expected
    done
  done

(*
 * Check value limits.
 *)

let check_value_limits_test _ =
  let a = Array.init 387 (fun i -> i - 129) in
  let limits b =
    Array.fold_left
      (fun (mini,maxi) i ->
         try
           ignore (b i);
           (min mini i, max maxi i)
         with
           _ -> (mini, maxi))
      (0,0)
      a
  in
  assert_equal
    (List.map limits [
      (fun i -> [%bitstring {| i : 2 : signed |}]);
      (fun i -> [%bitstring {| i : 3 : signed |}]);
      (fun i -> [%bitstring {| i : 4 : signed |}]);
      (fun i -> [%bitstring {| i : 5 : signed |}]);
      (fun i -> [%bitstring {| i : 6 : signed |}]);
      (fun i -> [%bitstring {| i : 7 : signed |}]);
      (fun i -> [%bitstring {| i : 8 : signed |}]);
    ])
    [
      (-2, 3);
      (-4, 7);
      (-8, 15);
      (-16, 31);
      (-32, 63);
      (-64, 127);
      (-128, 255)
    ]

(*
 * Signed byte create.
 *)

let signed_byte_create_test _ =
  let a n =
    let n' = 1 lsl (pred n) in
    Array.to_list (Array.init n' (fun i -> -(n'-i), n'+i)) @
    Array.to_list (Array.init (n' lsl 1) (fun i -> i,i))
  in
  let t s i =
    List.fold_left
      (fun ok (n,c) -> s n =  String.make 1 (Char.chr (c lsl (8-i))) && ok )
      true
      (a i)
  in
  let ok = fst (List.fold_left (fun (ok,i) s ->
      t s i && ok, succ i) (true, 2)
      [
        (fun i -> Bitstring.string_of_bitstring [%bitstring {| i : 2 : signed |}]);
        (fun i -> Bitstring.string_of_bitstring [%bitstring {| i : 3 : signed |}]);
        (fun i -> Bitstring.string_of_bitstring [%bitstring {| i : 4 : signed |}]);
        (fun i -> Bitstring.string_of_bitstring [%bitstring {| i : 5 : signed |}]);
        (fun i -> Bitstring.string_of_bitstring [%bitstring {| i : 6 : signed |}]);
        (fun i -> Bitstring.string_of_bitstring [%bitstring {| i : 7 : signed |}]);
        (fun i -> Bitstring.string_of_bitstring [%bitstring {| i : 8 : signed |}]);
      ])
  in
  assert_equal ok true

(*
 * Signed byte create and match
 *)

let signed_byte_create_and_match_test _ =
  let a n =
    let n' = 1 lsl (pred n) in
    Array.to_list (Array.init (n' lsl 1) (fun i -> i-n'))
  in

  let t s i =
    List.fold_left
      (fun ok n -> s n = n && ok )
      true
      (a i)
  in
  let ok = fst (List.fold_left (fun (ok,i) s ->
      t s i && ok, succ i) (true, 2)
      [
        (fun n -> match%bitstring [%bitstring {| n : 2 : signed |}] with {| i : 2 : signed |} -> i | {| _ |} -> assert false);
        (fun n -> match%bitstring [%bitstring {| n : 3 : signed |}] with {| i : 3 : signed |} -> i | {| _ |} -> assert false);
        (fun n -> match%bitstring [%bitstring {| n : 4 : signed |}] with {| i : 4 : signed |} -> i | {| _ |} -> assert false);
        (fun n -> match%bitstring [%bitstring {| n : 5 : signed |}] with {| i : 5 : signed |} -> i | {| _ |} -> assert false);
        (fun n -> match%bitstring [%bitstring {| n : 6 : signed |}] with {| i : 6 : signed |} -> i | {| _ |} -> assert false);
        (fun n -> match%bitstring [%bitstring {| n : 7 : signed |}] with {| i : 7 : signed |} -> i | {| _ |} -> assert false);
        (fun n -> match%bitstring [%bitstring {| n : 8 : signed |}] with {| i : 8 : signed |} -> i | {| _ |} -> assert false);
      ])
  in
  assert_equal ok true

(*
 * Signed int limits
 *)

let signed_int_limits_test _ =
  Random.self_init ();
  let res = List.fold_left
      (fun (ok, i) (b, m) ->
         let above_maxp = 1 lsl i in
         let maxp = pred above_maxp in
         let minp = - (above_maxp lsr 1) in
         let below_minp = pred minp in
         let gut =
           try ignore (b maxp); true
           with _ -> false in
         let gut2 =
           try ignore (b above_maxp); false
           with _ -> true in
         let gut3 =
           try ignore (b minp); true
           with _ -> false in
         let gut4 =
           try ignore (b below_minp); false
           with _ -> true in
         let gut5 =
           let plage = Int32.shift_left 1l i in
           let test () =
             let signed_number =
               Int32.to_int (Int32.add (Random.int32 plage) (Int32.of_int minp)) in
             let bits = b signed_number in
             let number' = m bits in
             if signed_number = number' then true
             else
               begin
                 Printf.printf "bits:%d n=%x read=%x (%x %x)\n" i signed_number number' minp maxp;
                 false
               end in
           let res = ref true in
           for i = 1 to 10_000 do
             res := !res && test ()
           done;
           !res
         in
         (gut && gut2 && gut3 && gut4 && gut5 && ok, succ i)
      )
      (true, 9)
      [
        (fun n -> [%bitstring {| n : 9 : signed |}]),
        (fun b -> match%bitstring b with {| n: 9 : signed |} -> n);
        (fun n -> [%bitstring {| n : 10 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 10 : signed |} -> n);
        (fun n -> [%bitstring {| n : 11 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 11 : signed |} -> n);
        (fun n -> [%bitstring {| n : 12 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 12 : signed |} -> n);
        (fun n -> [%bitstring {| n : 13 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 13 : signed |} -> n);
        (fun n -> [%bitstring {| n : 14 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 14 : signed |} -> n);
        (fun n -> [%bitstring {| n : 15 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 15 : signed |} -> n);
        (fun n -> [%bitstring {| n : 16 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 16 : signed |} -> n);
        (fun n -> [%bitstring {| n : 17 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 17 : signed |} -> n);
        (fun n -> [%bitstring {| n : 18 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 18 : signed |} -> n);
        (fun n -> [%bitstring {| n : 19 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 19 : signed |} -> n);
        (fun n -> [%bitstring {| n : 20 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 20 : signed |} -> n);
        (fun n -> [%bitstring {| n : 21 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 21 : signed |} -> n);
        (fun n -> [%bitstring {| n : 22 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 22 : signed |} -> n);
        (fun n -> [%bitstring {| n : 23 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 23 : signed |} -> n);
        (fun n -> [%bitstring {| n : 24 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 24 : signed |} -> n);
        (fun n -> [%bitstring {| n : 25 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 25 : signed |} -> n);
        (fun n -> [%bitstring {| n : 26 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 26 : signed |} -> n);
        (fun n -> [%bitstring {| n : 27 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 27 : signed |} -> n);
        (fun n -> [%bitstring {| n : 28 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 28 : signed |} -> n);
        (fun n -> [%bitstring {| n : 29 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 29 : signed |} -> n);
        (fun n -> [%bitstring {| n : 30 : signed |}]),
        (fun b -> match%bitstring b with  {| n : 30 : signed |} -> n);
      ]
  in
  assert_equal (fst res) true;
  begin try
      if Sys.word_size = 32 then
        begin
          ignore ([%bitstring {| max_int : 31 : signed |}]);
          ignore ([%bitstring {| min_int : 31 : signed |}]);
        end
      else
        begin
          ignore ([%bitstring {| pred (1 lsl 31) : 31 : signed |}]);
          ignore ([%bitstring {| (-1 lsl 30) : 31 : signed |}]);
        end;
    with
      _ -> assert_failure "Second test failed"
  end;
  if Sys.word_size = 64 then
    try
      ignore ([%bitstring {| 1 lsl 31 : 31 : signed |}]);
      ignore ([%bitstring {| pred (-1 lsl 30) : 31 : signed |}]);
      assert_failure "Third test failed"
    with _ ->
      ()

(*
 * Test functions which construct and extract fixed-length ints of various
 * sizes. Manquent les tests random pour bits = 31
 *)

let fixed_extraction_test _ =
  for i = 0 to 129 do
    let zeroes = Bitstring.zeroes_bitstring i in
    let%bitstring bits = {|
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
    |}
    in
    match%bitstring bits with
    | {| _ : i : bitstring;
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
      |} ->
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
    | {| _ |} ->
      failwith "15_extract_int"
  done

(*
 * Test fix for a regression when extracting 32 and 64 bit aligned integers
 * (discovered / fixed / tested by Hans Ole Rafaelsen).
 *)

let bitstring_of_int32 i =
  [%bitstring {| i : 32 |}]

let bitstring_of_int64 i =
  [%bitstring {| i : 64 |}]

let int32_of_bitstring bits =
  match%bitstring bits with
  | {| i : 32 |} -> i

let int64_of_bitstring bits =
  match%bitstring bits with
  | {| i : 64 |} -> i

let extract_regression_test _ =
  let b1 = bitstring_of_int32 1_l in
  let b2 = bitstring_of_int32 2_l in
  let b3 = bitstring_of_int32 3_l in
  let i1 = int32_of_bitstring b1 in
  let i2 = int32_of_bitstring b2 in
  let i3 = int32_of_bitstring b3 in
  assert (i1 = 1_l);
  assert (i2 = 2_l);
  assert (i3 = 3_l);

  let b1 = bitstring_of_int64 1_L in
  let b2 = bitstring_of_int64 2_L in
  let b3 = bitstring_of_int64 3_L in
  let i1 = int64_of_bitstring b1 in
  let i2 = int64_of_bitstring b2 in
  let i3 = int64_of_bitstring b3 in
  assert (i1 = 1_L);
  assert (i2 = 2_L);
  assert (i3 = 3_L)

(*
 * Construct and match against random variable sized strings.
 *)

let nr_passes = 10000
let max_size = 8			(* max field size in bits *)

(* let () = Bitstring.debug := true *)

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
  eprintf "bits (length = %d):\n" (Bitstring.bitstring_length bits);
  Bitstring.hexdump_bitstring stderr bits;
  eprintf "%!"

let construct_and_match_random_test _ =
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
        [%bitstring {|
            n0 : n0sz;
            n1 : n1sz;
            n2 : n2sz;
            n3 : n3sz
               |}]
      with
        Bitstring.Construct_failure (msg, _, _, _) ->
        eprintf "FAILED: Construct_failure %s\n%!" msg;
        dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz
          (Bitstring.empty_bitstring) 0L 0L 0L 0L;
        exit 2
    in

    let r0, r1, r2, r3 =
      match%bitstring bits with
      | {| r0 : n0sz; r1 : n1sz; r2 : n2sz; r3 : n3sz; rest : -1 : bitstring |} ->
        let rest_len = Bitstring.bitstring_length rest in
        if rest_len <> 0 then (
          eprintf "FAILED: rest is not zero length (length = %d)\n%!"
            rest_len;
          dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits 0L 0L 0L 0L;
          exit 2
        );
        r0, r1, r2, r3
      | {| _ |} ->
        eprintf "FAILED: match operator did not match\n%!";
        dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits 0L 0L 0L 0L;
        exit 2 in

    (*dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits r0 r1 r2 r3;*)

    if n0 <> r0 || n1 <> r1 || n2 <> r2 || n3 <> r3 then (
      eprintf "FAILED: numbers returned from match are different\n%!";
      dump n0 n0sz n1 n1sz n2 n2sz n3 n3sz bits r0 r1 r2 r3;
      exit 2
    )
  done

(*
 * Test the Bitstring.Buffer module and string_of_bitstring in nasty non-aligned
 * corner cases.
 *)

let nasty_non_aligned_corner_case_test _ =
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

      assert_equal (Bitstring.bitstring_length subbits) len;

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

(*
 * Test concat and the bit get functions.
 *)

let concat_bit_get_test _ =
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

(*
 * Compare bitstrings.
 *)

let sgn = function
  | 0 -> 0
  | i when i > 0 -> 1
  | _ -> -1

let compare_test _ =
  for i = 0 to 33 do
    for j = 0 to 33 do
      let bits1 = Bitstring.ones_bitstring i
      and bits2 = Bitstring.ones_bitstring j in
      let r = Bitstring.compare bits1 bits2 in
      if sgn r <> sgn (compare i j) then (
        eprintf "ones compare failed %d %d %d\n" i j r;
        exit 1
      )
    done
  done;
  for i = 0 to 33 do
    for j = 0 to 33 do
      let bits1 = Bitstring.zeroes_bitstring i
      and bits2 = Bitstring.zeroes_bitstring j in
      let r = Bitstring.compare bits1 bits2 in
      if sgn r <> sgn (compare i j) then (
        eprintf "zeroes compare failed %d %d %d\n" i j r;
        exit 1
      )
    done
  done;
  for i = 0 to 33 do
    for j = 0 to 33 do
      let bits1 = Bitstring.make_bitstring i '\x55'
      and bits2 = Bitstring.make_bitstring j '\x55' in
      let r = Bitstring.compare bits1 bits2 in
      if sgn r <> sgn (compare i j) then (
        eprintf "x55 compare failed %d %d %d\n" i j r;
        exit 1
      )
    done
  done;
  for i = 0 to 33 do
    for j = 0 to 33 do
      let bits1 = Bitstring.make_bitstring i '\x55' in
      let bits2 = Bitstring.make_bitstring i '\x55' in
      let bits2 = Bitstring.concat [Bitstring.zeroes_bitstring j; bits2] in
      assert (Bitstring.bitstring_length bits2 = j+i);
      let bits2 = Bitstring.dropbits j bits2 in
      assert (Bitstring.bitstring_length bits2 = i);
      let r = Bitstring.compare bits1 bits2 in
      if r <> 0 then (
        eprintf "x55 non-aligned compare failed %d %d %d\n" i j r;
        exit 1
      )
    done
  done

(*
 * Test subbitstring call.
 *)

let subbitstring_test _ =
  let bits = Bitstring.make_bitstring 65 '\x5a' in
  for off = 0 to 65 do
    for len = 65-off to 0 do
      let sub = Bitstring.subbitstring bits off len in
      for i = 0 to len-1 do
        if Bitstring.get bits (off+i) <> Bitstring.get sub i then (
          eprintf "33_substring: failed %d %d %d\n" off len i;
          exit 1
        )
      done
    done
  done

(*
 * Test takebits call.
 *)

let takebits_test _ =
  let bits = Bitstring.make_bitstring 65 '\x5a' in
  for len = 0 to 65 do
    let sub = Bitstring.takebits len bits in
    assert (Bitstring.bitstring_length sub = len)
  done

(*
 * Test the various functions to load bitstrings from files.
 *)

let file_load_test _ =
  let bits1 =
    let b1 = Bitstring.make_bitstring 800 '\x5a' in
    let b2 = Bitstring.make_bitstring 400 '\x88' in (
      [%bitstring {|
        b1 : 800 : bitstring;
        b2 : 400 : bitstring
           |}]
    ) in
  let bits2 = (
    let b = Bitstring.make_bitstring 800 '\xaa' in
    [%bitstring {|
      b : 800 : bitstring
        |}]
  ) in
  let bits = Bitstring.concat [bits1; bits2] in
  let filename, chan =
    Filename.open_temp_file ~mode:[Open_binary] "bitstring_test" ".tmp" in
  Bitstring.bitstring_to_chan bits chan;
  close_out chan;

  let bits' = Bitstring.bitstring_of_file filename in
  assert (Bitstring.equals bits bits');

  let chan = open_in filename in
  let bits' = Bitstring.bitstring_of_chan chan in
  close_in chan;
  assert (Bitstring.equals bits bits');

  let chan = open_in filename in
  let bits' = Bitstring.bitstring_of_chan_max chan 150 in
  assert (Bitstring.equals bits1 bits');
  let bits' = Bitstring.bitstring_of_chan_max chan 100 in
  assert (Bitstring.equals bits2 bits');
  close_in chan;

  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  let bits' = Bitstring.bitstring_of_file_descr fd in
  Unix.close fd;
  assert (Bitstring.equals bits bits');

  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  let bits' = Bitstring.bitstring_of_file_descr_max fd 150 in
  assert (Bitstring.equals bits1 bits');
  let bits' = Bitstring.bitstring_of_file_descr_max fd 100 in
  assert (Bitstring.equals bits2 bits');
  Unix.close fd;

  Unix.unlink filename

(*
 * Test if bitstrings are all zeroes or all ones.
 *)

let zeroes_ones_test _ =
  for i = 0 to 33 do
    let bits = Bitstring.zeroes_bitstring i in
    if not (Bitstring.is_zeroes_bitstring bits) then (
      eprintf "is_zeros_bitstring failed %d\n" i;
      exit 1
    );
    if i > 0 && Bitstring.is_ones_bitstring bits then (
      eprintf "false match is_ones_bitstring %d\n" i;
      exit 1
    )
  done;
  for i = 0 to 33 do
    let bits = Bitstring.ones_bitstring i in
    if not (Bitstring.is_ones_bitstring bits) then (
      eprintf "is_ones_bitstring failed %d\n" i;
      exit 1
    );
    if i > 0 && Bitstring.is_zeroes_bitstring bits then (
      eprintf "false match is_zeroes_bitstring %d\n" i;
      exit 1
    )
  done

(*
 * Endianness expressions
 *)

let endianness_test _ =
  let rec loop = function
    | (e, expected) :: rest ->
      let%bitstring bits = {|
          expected : 32 : endian (e);
          expected : 32 : endian (e);
          expected : 32 : endian (e)
                   |} in
      (match%bitstring bits with
       | {| actual : 32 : endian (e);
            actual : 32 : endian (e);
            actual : 32 : endian (e) |} ->
         if actual <> expected then
           failwith (sprintf "actual %ld <> expected %ld" actual expected)
       | {| _ |} as bits ->
         Bitstring.hexdump_bitstring stderr bits; exit 1
      );
      loop rest
    | [] -> ()
  in
  loop [
    Bitstring.BigEndian, 0xa1b2c3d4_l;
    Bitstring.BigEndian, 0xa1d4c3b2_l;
    Bitstring.LittleEndian, 0xa1b2c3d4_l;
    Bitstring.LittleEndian, 0xa1d4c3b2_l;
    Bitstring.NativeEndian, 0xa1b2c3d4_l;
    Bitstring.NativeEndian, 0xa1d4c3b2_l;
  ]

(*
 * Simple offset test
 *)

let simple_offset_test _ =
  let make_bits i n j m k = (
    let pad1 = Bitstring.ones_bitstring (n-8) in
    let pad2 = Bitstring.ones_bitstring (m-n-8) in
    [%bitstring {|
    i : 8;
    pad1 : n-8 : bitstring;
    j : 8;			     (* this should be at offset(n) *)
    pad2 : m-n-8 : bitstring;
    k : 8			     (* this should be at offset(m) *)
      |}]
  )
  in
  let test_bits bits i n j m k =
    match%bitstring bits with
    | {| i' : 8;
         j' : 8 : offset(n);
         k' : 8 : offset(m) |} when i = i' && j = j' && k = k' -> () (* ok *)
    | {| _ |} ->
      failwith (sprintf "60_simple_offset: test_bits: failed %d %d %d %d %d"
                  i n j m k)
  in
  for n = 8 to 128 do
    for m = n+8 to 256 do
      List.iter (fun (i,j,k) -> test_bits (make_bits i n j m k) i n j m k)
        [0x55, 0xaa, 0x33; 0x33, 0xaa, 0x55; 0x12, 0x34, 0x56]
    done;
  done

(*
 * Offset string. The rotation functions used for strings are
 * very complicated so this is worth testing separately.
 *)

let offset_string_test _ =
  let make_bits si n sj m sk = (
    let pad1 = Bitstring.ones_bitstring (n-64) in
    let pad2 = Bitstring.ones_bitstring (m-n-8) in
    [%bitstring {|
    si : 64 : string;
    pad1 : n-64 : bitstring;
    sj : 8 : string;		     (* this should be at offset(n) *)
    pad2 : m-n-8 : bitstring;
    sk : 64 : string		     (* this should be at offset(m) *)
       |}]
  )
  in
  let test_bits bits si n sj m sk =
    match%bitstring bits with
    | {| si' : 64 : string;
         sj' : 8 : string, offset(n);
         sk' : 64 : string, offset(m) |}
      when si = si' && sj = sj' && sk = sk' -> () (* ok *)
    | {| _ |} ->
      failwith (sprintf "61_offset_string: test_bits: failed %S %d %S %d %S"
                  si n sj m sk)
  in
  for n = 64 to 128 do
    for m = n+8 to 256 do
      List.iter (fun (si,sj,sk) ->
          test_bits (make_bits si n sj m sk) si n sj m sk)
        ["ABCDEFGH", "x", "HGFEDCBA";
         "01234567", "0", "76543210";
         "abcdefgh", "\x55", "poiuytre"]
    done;
  done

(*
 * Test computed offsets when original_off <> 0.
 *)

let computed_offset_test _ =
  let make_bits p i n j m k =
    let pad0 = Bitstring.ones_bitstring p in
    let pad1 = Bitstring.ones_bitstring (n-8) in
    let pad2 = Bitstring.ones_bitstring (m-n-8) in
    [%bitstring {|
      pad0 : p     : bitstring;	  (* will be skipped below *)
      i    : 8;
      pad1 : n-8   : bitstring;
      j    : 8;			              (* this should be at offset(n) *)
      pad2 : m-n-8 : bitstring;
      k    : 8			              (* this should be at offset(m) *)
    |}]
  in
  let test_bits bits p i n j m k =
    (*
     * Skip the 'p' padding bits so the match starts at a non-zero offset.
     *)
    let bits = Bitstring.dropbits p bits
    in
    match%bitstring bits with
    | {| i' : 8;
         j' : 8 : offset(n);
         k' : 8 : offset(m)
      |} when i = i' && j = j' && k = k' -> () (* ok *)
    | {| i' : 8;
         j' : 8 : offset(n);
         k' : 8 : offset(m)
      |} ->
      Printf.printf "\n%d %d %d\n" p n m;
      Bitstring.hexdump_bitstring stdout bits;
      Printf.printf "%x %x\n" i i';
      assert_equal i i';
      Printf.printf "%x %x\n" j j';
      assert_equal j j';
      Printf.printf "%x %x\n" k k';
      assert_equal k k'
    | {| _ |} ->
      assert_failure "Bitstring parsing failure"
  in
  for p = 1 to 4 do
    for n = 8 to 128 do
      for m = n+8 to 256 do
        List.iter (fun (i,j,k) -> test_bits (make_bits p i n j m k) p i n j m k)
          [0x55, 0xaa, 0x33; 0x33, 0xaa, 0x55; 0x12, 0x34, 0x56]
      done;
    done;
  done

(*
 * Test save_offset_to.
 *)

let save_offset_to_test _ =
  let make_bits p i n j m k = (
    let pad0 = Bitstring.ones_bitstring p in
    let pad1 = Bitstring.ones_bitstring (n-8) in
    let pad2 = Bitstring.ones_bitstring (m-n-8) in
    [%bitstring {|
    pad0 : p : bitstring;	     (* will be skipped below *)
    i : 8;
    pad1 : n-8 : bitstring;
    j : 8;			     (* this should be at offset(n) *)
    pad2 : m-n-8 : bitstring;
    k : 8			     (* this should be at offset(m) *)
      |}]
  )
  in
  let test_bits bits p i n j m k =
    (* Skip the 'p' padding bits so the match starts at a non-zero offset. *)
    let bits = Bitstring.dropbits p bits in

    match%bitstring bits with
    | {| i' : 8;
         _ : n-8 : bitstring;
         j' : 8 : save_offset_to (j_offset);
         _ : m-n-8 : bitstring;
         k' : 8 : save_offset_to (k_offset) |}
      when i = i' && j = j' && k = k' && j_offset = n && k_offset = m ->
      () (* ok *)
    | {| _ |} ->
      failwith (sprintf
                  "65_save_offset_to: test_bits: failed %d %d %d %d %d %d"
                  p i n j m k)
  in
  for p = 0 to 4 do
    for n = 8 to 64 do
      for m = n+8 to 128 do
        List.iter (fun (i,j,k) -> test_bits (make_bits p i n j m k) p i n j m k)
          [0x55, 0xaa, 0x33; 0x33, 0xaa, 0x55; 0x12, 0x34, 0x56]
      done;
    done;
  done

(*
 * Test check() and bind().
 *)

let check_bind_test _ =
  let%bitstring bits = {| 101 : 16; 202 : 16 |}
  in
  match%bitstring bits with
  | {| i : 16 : check (i = 101), bind (i*4);
       j : 16 : check (j = 202) |} ->
    if i <> 404 || j <> 202 then
      failwith (sprintf "70_check_and_bind: failed: %d %d" i j)
  | {| _ |} ->
    failwith "70_check_and_bind: match failed"

(*
 * Test hexdump.
 *)

let () =
  let diff = "diff"
  in
  let files = Sys.readdir "../../../tests/data" in
  let files = Array.to_list files in
  let files = List.filter (
      fun filename ->
        String.length filename > 3 &&
        filename.[0] = 'r' && filename.[1] = 'n' && filename.[2] = 'd'
    ) files in
  let files = List.map (
      fun filename ->
        let n = String.sub filename 3 (String.length filename - 3) in
        let n = int_of_string n in
        let bits = Bitstring.bitstring_of_file ("../../../tests/data/" ^ filename) in
        (*
         * 'bitstring_of_file' loads whole bytes.  Truncate it to
         * the real bit-length.
         *)
        let bits = Bitstring.takebits n bits in
        filename, n, bits
    ) files in
  (*
   * Hexdump the bits, then compare using external 'diff' program.
   *)
  List.iter (
    fun (filename, n, bits) ->
      let output_filename = sprintf "../../../tests/data/hex%d.actual" n in
      let chan = open_out output_filename in
      Bitstring.hexdump_bitstring chan bits;
      close_out chan
  ) files;

  List.iter (
    fun (filename, n, bits) ->
      let actual_filename = sprintf "../../../tests/data/hex%d.actual" n in
      let expected_filename = sprintf "../../../tests/data/hex%d.expected" n in
      let cmd =
        sprintf "%s -u %s %s"
          (Filename.quote diff)
          (Filename.quote expected_filename)
          (Filename.quote actual_filename) in
      if Sys.command cmd <> 0 then (
        exit 1
      )
  ) files

(*
 * Regression test for bug in 'as-binding' found by Matej Kosik.
 * $Id$
 *)

let as_binding_bug_test _ =
  let bits = Bitstring.ones_bitstring 1 in
  match%bitstring bits with
  | {| _ : 1 |} as foo ->
    let len = Bitstring.bitstring_length foo in
    if len <> 1 then (
      Bitstring.hexdump_bitstring stderr foo;
      eprintf "test error: length = %d, expecting 1\n" len;
      exit 1
    )
  | {| _ |} ->
    assert false

(*
 * Regression test for bug in concatenation found by Phil Tomson.
 *)

let concat_regression_test _ =
  let errors = ref 0 in
  let bs_256 = Bitstring.ones_bitstring 256 in
  assert (Bitstring.bitstring_length bs_256 = 256);

  let%bitstring bs2 = {|
    false : 1;
    (Bitstring.subbitstring bs_256 0 66) : 66 : bitstring
  |} in
  let len = Bitstring.bitstring_length bs2 in
  if len <> 67 then (
    eprintf "invalid length of bs2: len = %d, expected 67\n" len;
    Bitstring.hexdump_bitstring stderr bs2;
    incr errors
  );

  let%bitstring bs3 = {|
    false : 1;
    (Bitstring.subbitstring bs_256 0 66) : 66 : bitstring;
    (Bitstring.subbitstring bs_256 66 67) : 67 : bitstring
  |} in
  let len = Bitstring.bitstring_length bs3 in
  if len <> 134 then (
    eprintf "invalid length of bs3: len = %d, expected 134\n" len;
    Bitstring.hexdump_bitstring stderr bs3;
    incr errors
  );

  let%bitstring bs4 = {|
    (Bitstring.subbitstring bs_256 66 67) : 67 : bitstring
  |} in
  let len = Bitstring.bitstring_length bs4 in
  if len <> 67 then (
    eprintf "invalid length of bs4: len = %d, expected 67\n" len;
    Bitstring.hexdump_bitstring stderr bs4;
    incr errors
  );

  let bs5 = Bitstring.concat [Bitstring.subbitstring bs_256 0 66; Bitstring.subbitstring bs_256 66 67] in
  let len = Bitstring.bitstring_length bs5 in
  if len <> 133 then (
    eprintf "invalid length of bs5: len = %d, expected 133\n" len;
    Bitstring.hexdump_bitstring stderr bs5;
    incr errors
  );

  let bs6 = Bitstring.concat [ Bitstring.subbitstring bs_256 0 64; Bitstring.subbitstring bs_256 64 64] in
  let len = Bitstring.bitstring_length bs6 in
  if len <> 128 then (
    eprintf "invalid length of bs6: len = %d, expected 128\n" len;
    Bitstring.hexdump_bitstring stderr bs6;
    incr errors
  );

  let bs7 = Bitstring.concat [ Bitstring.subbitstring bs_256 0 65; Bitstring.subbitstring bs_256 65 64] in
  let len = Bitstring.bitstring_length bs7 in
  if len <> 129 then (
    eprintf "invalid length of bs7: len = %d, expected 129\n" len;
    Bitstring.hexdump_bitstring stderr bs7;
    incr errors
  );

  if !errors <> 0 then exit 1

let suite = "BitstringLegacyTests" >::: [
    "load_test"                          >:: load_test;
    "run_test"                           >:: run_test;
    "match_random_bits_test"             >:: match_random_bits_test;
    "match_random_bits_with_int_test"    >:: match_random_bits_with_int_test;
    "check_value_limits_test"            >:: check_value_limits_test;
    "signed_byte_create_test"            >:: signed_byte_create_test;
    "signed_byte_create_and_match_test"  >:: signed_byte_create_and_match_test;
    "signed_int_limits_test"             >:: signed_int_limits_test;
    "fixed_extraction_test"              >:: fixed_extraction_test;
    "extract_regression_test"            >:: extract_regression_test;
    "construct_and_match_random_test"    >:: construct_and_match_random_test;
    "nasty_non_aligned_corner_case_test" >:: nasty_non_aligned_corner_case_test;
    "concat_bit_get_test"                >:: concat_bit_get_test;
    "compare_test"                       >:: compare_test;
    "subbitstring_test"                  >:: subbitstring_test;
    "takebits_test"                      >:: takebits_test;
    "file_load_test"                     >:: file_load_test;
    "zeroes_ones_test"                   >:: zeroes_ones_test;
    "endianness_test"                    >:: endianness_test;
    "simple_offset_test"                 >:: simple_offset_test;
    "offset_string_test"                 >:: offset_string_test;
    "computed_offset_test"               >:: computed_offset_test;
    "save_offset_to_test"                >:: save_offset_to_test;
    "check_bind_test"                    >:: check_bind_test;
    "as_binding_bug_test"                >:: as_binding_bug_test;
    "concat_regression_test"             >:: concat_regression_test;
  ]
