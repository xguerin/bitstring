let a = Array.init 387 (fun i -> i - 129)

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

let () =
  if
    List.map limits [
      (fun i -> BITSTRING { i : 2 : signed });
      (fun i -> BITSTRING { i : 3 : signed });
      (fun i -> BITSTRING { i : 4 : signed });
      (fun i -> BITSTRING { i : 5 : signed });
      (fun i -> BITSTRING { i : 6 : signed });
      (fun i -> BITSTRING { i : 7 : signed });
      (fun i -> BITSTRING { i : 8 : signed });
    ]
    <>
      [
	(-2, 3); 
	(-4, 7); 
	(-8, 15); 
	(-16, 31); 
	(-32, 63); 
	(-64, 127); 
	(-128, 255)
      ]
  then
    failwith("t12_signed_bytes_limits: failed")
