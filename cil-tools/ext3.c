/* This is an example import file, showing how to import the ext3
 * superblock automatically from Linux header files.
 *
 * Use: bitmatch-import-c ext3.c
 *
 * Tip: Add the --debug flag to that command line to see what's going on.
 */

/* These are required by Linux in order to get the little/big-endian
 * notations present in the Linux kernel header files.  Any other
 * defines needed can go here.
 */
#define __CHECKER__      1
#define __CHECK_ENDIAN__ 1

/* Include files necessary to get the structure(s) and constant(s) we're
 * interested in.
 */
#include <linux/magic.h>
#include <linux/ext3_fs.h>

/* This tells the importer program what structures and constants to import. */
typedef struct ext3_super_block BITMATCH_IMPORT(ext3_super_block);
BITMATCH_CONSTANT_INT32 (ext3_super_magic, EXT3_SUPER_MAGIC);
