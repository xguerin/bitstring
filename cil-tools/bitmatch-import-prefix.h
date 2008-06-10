/* This file is automagically prefixed to all .ubd files
 * and provides some macros that we need for CIL.
 */

#define BITMATCH_IMPORT(name) __bitmatch_import_##name
#define BITMATCH_CONSTANT_STRING(name,val) \
  char *__bitmatch_constant_##name = val
#define BITMATCH_CONSTANT_INT32(name,val) \
  int __bitmatch_constant_##name = val
