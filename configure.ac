# Bitmatch syntax extension.
# Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

dnl Process this file with autoconf to produce a configure script.

AC_INIT(ocaml-bitmatch,1.9.2)

dnl Check for basic C environment.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_CPP

AC_C_PROTOTYPES
test "x$U" != "x" && AC_MSG_ERROR(Compiler not ANSI compliant)

AC_PROG_CC_C_O

dnl Check for native endianness.
AC_C_BIGENDIAN(
  [NATIVEENDIAN=BigEndian],
  [NATIVEENDIAN=LittleEndian],
  [AC_MSG_ERROR([Machine endianness could not be determined])]
)
AC_SUBST(NATIVEENDIAN)

dnl Check for basic OCaml environment & findlib.
AC_PROG_OCAML
AC_PROG_FINDLIB

if test "x$OCAMLFIND" = "x"; then
    AC_MSG_ERROR([You must have ocaml and findlib installed])
fi

dnl Check for camlp4
AC_CHECK_PROG(CAMLP4OF,camlp4of.opt,camlp4of.opt)
if test "x$CAMLP4OF" = "xno"; then
    AC_MSG_ERROR([You must have camlp4 installed])
fi

dnl Check for CIL (optional)
AC_CHECK_OCAML_PKG(cil)
AC_SUBST(pkg_cil)

dnl Produce output files.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile META bitmatch_config.ml cil-tools/Makefile])
AC_OUTPUT
