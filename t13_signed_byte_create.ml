let a n =
  let n' = 1 lsl (pred n) in
     Array.to_list (Array.init n' (fun i -> -(n'-i), n'+i)) @
      Array.to_list (Array.init (n' lsl 1) (fun i -> i,i));;

let t s i =
    List.fold_left 
      (fun ok (n,c) -> s n =  String.make 1 (Char.chr (c lsl (8-i))) && ok )
      true
      (a i);;

let ok = fst (List.fold_left (fun (ok,i) s ->
		    t s i && ok, succ i) (true, 2)
  [
    (fun i -> Bitstring.string_of_bitstring (BITSTRING { i : 2 : signed }));
    (fun i -> Bitstring.string_of_bitstring (BITSTRING { i : 3 : signed }));
    (fun i -> Bitstring.string_of_bitstring (BITSTRING { i : 4 : signed }));
    (fun i -> Bitstring.string_of_bitstring (BITSTRING { i : 5 : signed }));
    (fun i -> Bitstring.string_of_bitstring (BITSTRING { i : 6 : signed }));
    (fun i -> Bitstring.string_of_bitstring (BITSTRING { i : 7 : signed }));
    (fun i -> Bitstring.string_of_bitstring (BITSTRING { i : 8 : signed }));
  ])

in
if not ok then
  failwith("t13_signed_byte_create: failed")
