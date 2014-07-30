let a n =
  let n' = 1 lsl (pred n) in
    Array.to_list (Array.init (n' lsl 1) (fun i -> i-n'))

let t s i =
    List.fold_left 
      (fun ok n -> s n = n && ok )
      true
      (a i);;

let ok = fst (List.fold_left (fun (ok,i) s ->
		    t s i && ok, succ i) (true, 2)
[
  (fun n -> bitmatch BITSTRING { n : 2 : signed } with { i : 2 : signed } -> i | { _ } -> assert false); 
  (fun n -> bitmatch BITSTRING { n : 3 : signed } with { i : 3 : signed } -> i | { _ } -> assert false);
  (fun n -> bitmatch BITSTRING { n : 4 : signed } with { i : 4 : signed } -> i | { _ } -> assert false);
  (fun n -> bitmatch BITSTRING { n : 5 : signed } with { i : 5 : signed } -> i | { _ } -> assert false);
  (fun n -> bitmatch BITSTRING { n : 6 : signed } with { i : 6 : signed } -> i | { _ } -> assert false);
  (fun n -> bitmatch BITSTRING { n : 7 : signed } with { i : 7 : signed } -> i | { _ } -> assert false); 
  (fun n -> bitmatch BITSTRING { n : 8 : signed } with { i : 8 : signed } -> i | { _ } -> assert false);
])

in
if not ok then
  failwith("t13_signed_byte_create: failed")


