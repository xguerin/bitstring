(* Test a simple constructor.
 * $Id: 10_constr2.ml,v 1.2 2008-04-25 11:08:43 rjones Exp $
 *)

let version = 1 ;;
let data = 10 ;;
let bits =
  BITSTRING
    { version : 4;
      data : 12 } ;;

Bitmatch.hexdump_bitstring stdout bits ;;
