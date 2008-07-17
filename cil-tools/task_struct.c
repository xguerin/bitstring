/* This is an example import file, showing how to import the
 * Linux task_struct structure from header files.
 *
 * Use: bitstring-import-c task_struct.c > task_struct.ml
 *
 * Tip: Add the --debug flag to that command line to see what's going on.
 * Also use bitstring-objinfo to examine the bmpp file.
 */

/* Any defines, etc. necessary to get the include to work. */
#define __KERNEL__
#define CONFIG_HZ 100
#define CONFIG_PAGE_OFFSETUL 0xc0000000 /* XXX? */
#define THREAD_SIZE 4096 /* XXX? */

/* Include files necessary to get the structure(s) and constant(s) we're
 * interested in.
 *
 * Note in this case glibc strips the useful structures out of the
 * so-called "kernel headers" that it supplies, so instead I have
 * a local copy of the real headers from a version of Linux.
 */
#include "linux/sched.h"

/* This tells the importer program what structures and constants to import. */
typedef struct task_struct BITSTRING_IMPORT(task_struct);
