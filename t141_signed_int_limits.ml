let () = Random.self_init ();;


if not (
  fst (List.fold_left (fun (ok, i) (b,m) ->
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
			       Int32.to_int ( Int32.add (Random.int32 plage) (Int32.of_int minp) ) in
			     let bits = b signed_number in
			     let number' = m bits in
			       if signed_number = number' then true
			       else
				 begin
				   Printf.printf "bits:%d n=%d read=%d (%d %d)\n" i signed_number number' minp maxp;
				   false
				 end in
			   let res = ref true in
			     for i = 1 to 10_000 do
			       res := !res && test ()
			     done;
			     !res in
			   
			   (gut && gut2 && gut3 && gut4 && gut5 && ok, succ i)
			     
		      )
	 (true, 9)
			 [
			   (fun n -> BITSTRING { n : 9 : signed }),
			   (fun b -> bitmatch b with { n: 9 : signed } -> n);
			   (fun n -> BITSTRING { n : 10 : signed }),
			   (fun b -> bitmatch b with  { n : 10 : signed } -> n);
			   (fun n -> BITSTRING { n : 11 : signed }),
			   (fun b -> bitmatch b with  { n : 11 : signed } -> n);
			   (fun n -> BITSTRING { n : 12 : signed }),
			   (fun b -> bitmatch b with  { n : 12 : signed } -> n);
			   (fun n -> BITSTRING { n : 13 : signed }),
			   (fun b -> bitmatch b with  { n : 13 : signed } -> n);
			   (fun n -> BITSTRING { n : 14 : signed }),
			   (fun b -> bitmatch b with  { n : 14 : signed } -> n);
			   (fun n -> BITSTRING { n : 15 : signed }),
			   (fun b -> bitmatch b with  { n : 15 : signed } -> n);
			   (fun n -> BITSTRING { n : 16 : signed }),
			   (fun b -> bitmatch b with  { n : 16 : signed } -> n);
			   (fun n -> BITSTRING { n : 17 : signed }),
			   (fun b -> bitmatch b with  { n : 17 : signed } -> n);
			   (fun n -> BITSTRING { n : 18 : signed }),
			   (fun b -> bitmatch b with  { n : 18 : signed } -> n);
			   (fun n -> BITSTRING { n : 19 : signed }),
			   (fun b -> bitmatch b with  { n : 19 : signed } -> n);
			   (fun n -> BITSTRING { n : 20 : signed }),
			   (fun b -> bitmatch b with  { n : 20 : signed } -> n);
			   (fun n -> BITSTRING { n : 21 : signed }),
			   (fun b -> bitmatch b with  { n : 21 : signed } -> n);
			   (fun n -> BITSTRING { n : 22 : signed }),
			   (fun b -> bitmatch b with  { n : 22 : signed } -> n);
			   (fun n -> BITSTRING { n : 23 : signed }),
			   (fun b -> bitmatch b with  { n : 23 : signed } -> n);
			   (fun n -> BITSTRING { n : 24 : signed }),
			   (fun b -> bitmatch b with  { n : 24 : signed } -> n);
			   (fun n -> BITSTRING { n : 25 : signed }),
			   (fun b -> bitmatch b with  { n : 25 : signed } -> n);
			   (fun n -> BITSTRING { n : 26 : signed }),
			   (fun b -> bitmatch b with  { n : 26 : signed } -> n);
			   (fun n -> BITSTRING { n : 27 : signed }),
			   (fun b -> bitmatch b with  { n : 27 : signed } -> n);
			   (fun n -> BITSTRING { n : 28 : signed }),
			   (fun b -> bitmatch b with  { n : 28 : signed } -> n);
			   (fun n -> BITSTRING { n : 29 : signed }),
			   (fun b -> bitmatch b with  { n : 29 : signed } -> n);
			   (fun n -> BITSTRING { n : 30 : signed }),
			   (fun b -> bitmatch b with  { n : 30 : signed } -> n);
			 ]
      ) &&

    begin
      try
	if Sys.word_size = 32 then
	  begin
	    ignore (BITSTRING { max_int : 31 : signed });
	    ignore (BITSTRING { min_int : 31 : signed });
	  end
	else
	  begin
	    ignore (BITSTRING { pred (1 lsl 31) : 31 : signed });
	    ignore (BITSTRING { (-1 lsl 30) : 31 : signed });
	  end;
	true
      with 
	  _ ->
	    false;
    end

  &&

    begin
      if Sys.word_size = 64 then
	try
	  ignore (BITSTRING { 1 lsl 31 : 31 : signed });
	  ignore (BITSTRING { pred (-1 lsl 30) : 31 : signed });
	  false
	with _ -> true
      else
	true
    end

)
then
  failwith("t141_signed_int_limits: failed")


(* Manquent les tests random pour bits = 31 *)
