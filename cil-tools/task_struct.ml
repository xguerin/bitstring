(* This is an example program using an imported C structure.
 *
 * The Linux process table entry structure is imported from
 * <linux/sched.h> via "task_struct.c" by the bitmatch-import-c
 * program, and saved as "task_struct.bmpp".
 *
 * Then we can load "task_struct.bmpp" here.
 *)

open Printf

open bitmatch "task_struct.bmpp"

(*
let () =
  let bits = Bitmatch.bitstring_of_file "examples/ext3_sb" in
*)
