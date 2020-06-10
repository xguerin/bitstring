open OUnit2

let ( let* ) = Option.bind
let ( let+ ) x f = Option.map f x

let ( and* ) a b =
  let* a = a in
  let+ b = b in
  a, b
;;

let ( and+ ) = ( and* )

let match_bits_with_let_star_syntax _ =
  (let* bits = Some (Bitstring.bitstring_of_string "U") in
   Some
     (match%bitstring bits with
     | {| hi: 4; lo: 4 |} -> assert_equal hi lo
     | {| _ |} -> assert_failure "Something wen't terribly wrong!"))
  |> ignore
;;

let match_bits_with_let_plus_syntax _ =
  (let+ bits = Some (Bitstring.bitstring_of_string "U") in
   match%bitstring bits with
   | {| hi: 4; lo: 4 |} -> assert_equal hi lo
   | {| _ |} -> assert_failure "Something wen't terribly wrong!")
  |> ignore
;;

let match_bits_with_and_star_syntax _ =
  (let* s = Some 5
   and* bits = Some (Bitstring.bitstring_of_string "U") in
   Some
     (match%bitstring bits with
     | {| hi: 4; lo: 4 |} -> assert_equal lo s
     | {| _ |} -> assert_failure "Something wen't terribly wrong!"))
  |> ignore
;;

let match_bits_with_and_plus_syntax _ =
  (let* s = Some 5
   and+ bits = Some (Bitstring.bitstring_of_string "U") in
   Some
     (match%bitstring bits with
     | {| hi: 4; lo: 4 |} -> assert_equal lo s
     | {| _ |} -> assert_failure "Something wen't terribly wrong!"))
  |> ignore
;;

let suite =
  "BitstringLetStarSyntaxTest"
  >::: [ "match_bits_with_let_star_syntax" >:: match_bits_with_let_star_syntax
       ; "match_bits_with_let_plus_syntax" >:: match_bits_with_let_plus_syntax
       ; "match_bits_with_and_star_syntax" >:: match_bits_with_and_star_syntax
       ; "match_bits_with_and_plus_syntax" >:: match_bits_with_and_plus_syntax
       ]
;;
