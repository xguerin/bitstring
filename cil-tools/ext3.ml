(* This is an example program using an imported C structure.
 *
 * The C structure is imported from <linux/ext3_fs.h> via "ext3.c"
 * by the bitstring-import-c program, and saved as "ext3.bmpp".
 *
 * Then we can load "ext3.bmpp" here.
 *)

open Printf

open bitmatch "ext3.bmpp"

let () =
  (* Load a real ext3 superblock from the examples directory. *)
  let bits = Bitstring.bitstring_of_file "examples/ext3_sb" in

  bitmatch bits with
  | { :ext2_super_block } ->
      printf "ext3 superblock:\n";
      printf "  s_inodes_count = %ld\n" s_inodes_count;
      printf "  s_blocks_count = %ld\n" s_blocks_count;
      printf "  s_free_inodes_count = %ld\n" s_free_inodes_count;
      printf "  s_free_blocks_count = %ld\n" s_free_blocks_count;
      printf "  s_uuid = %S\n" s_uuid;
      printf "  s_volume_name = %S\n" s_volume_name;
      printf "  s_last_mounted = %S\n" s_last_mounted

  | { _ } ->
      failwith "input is not an ext2/3 superblock"
