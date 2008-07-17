/* This file is automagically prefixed to all .ubd files
 * and provides some macros that we need for CIL.
 *
 * This file is in the public domain.
 */

/* This is needed for older versions of CIL which didn't
 * support this C99 type.
 */
#ifndef _Bool
#define _Bool unsigned
#endif

#define BITSTRING_IMPORT(name) __bitstring_import_##name
#define BITSTRING_CONSTANT_STRING(name,val) \
  char *__bitstring_constant_##name = val
#define BITSTRING_CONSTANT_INT32(name,val) \
  int __bitstring_constant_##name = val
