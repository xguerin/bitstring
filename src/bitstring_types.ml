(*
 * Bitstring library.
 *
 * Copyright (C) 2008-2016 Red Hat Inc., Richard W.M. Jones
 * Copyright (C) 2016 Red Hat Inc, Richard W.M. Jones, Xavier R. Guerin.

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
 *)

type endian =
  | BigEndian
  | LittleEndian
  | NativeEndian

let string_of_endian = function
  | BigEndian -> "bigendian"
  | LittleEndian -> "littleendian"
  | NativeEndian -> "nativeendian"
;;
