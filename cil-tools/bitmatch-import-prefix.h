/* This file is automagically prefixed to all .ubd files
 * and provides some macros that we need for CIL.
 */

/* This is needed for older versions of CIL which didn't
 * support this C99 type.
 */
#ifndef _Bool
#define _Bool unsigned
#endif

#define BITMATCH_IMPORT(name) __bitmatch_import_##name
#define BITMATCH_CONSTANT_STRING(name,val) \
  char *__bitmatch_constant_##name = val
#define BITMATCH_CONSTANT_INT32(name,val) \
  int __bitmatch_constant_##name = val
