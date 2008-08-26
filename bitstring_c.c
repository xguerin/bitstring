/* Bitstring library.
 * Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version,
 * with the OCaml linking exception described in COPYING.LIB.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * $Id: bitstring.ml 146 2008-08-20 16:58:33Z richard.wm.jones $
 */

/* This file contains hand-coded, optimized C implementations of
 * certain very frequently used functions.
 */

#include <stdio.h>
#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>

/* Return a mask of 0-31 bits wide. */
CAMLprim value
ocaml_bitstring_I_mask (value bitsv)
{
  int bits = Int_val (bitsv);

  if (bits <= 31)
    return Val_int ((1 << bits) - 1);
  else
    caml_invalid_argument ("Bitstring.I.mask");
}
