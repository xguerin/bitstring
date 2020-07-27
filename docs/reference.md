# Reference

## Types

```ocaml
type endian =
| BigEndian
| LittleEndian
| NativeEndian
```

```ocaml
val string_of_endian : endian -> string
```

Endianness.

```ocaml
type bitstring = string * int * int
```

`bitstring` is the basic type used to store bitstrings.

The type contains the underlying data (a string), the current bit offset within
the string and the current bit length of the string (counting from the bit
offset). Note that the offset and length are in bits, not bytes.

Normally you don't need to use the bitstring type directly, since there are
functions and syntax extensions which hide the details.

See also `Bitstring.bitstring_of_string`, `Bitstring.bitstring_of_file`,
`Bitstring.hexdump_bitstring`, `Bitstring.bitstring_length`.

```ocaml
type t = bitstring
```

`t` is a synonym for the `Bitstring.bitstring type`. This allows you to use this
module with functors like Set and Map from the stdlib.

## Exceptions

```ocaml
exception Construct_failure of string * string * int * int
```

`Construct_failure (message, file, line, char)` may be raised by the `BITSTRING`
constructor. Common reasons are that values are out of range of the fields that
contain them, or that computed lengths are impossible (eg. negative length bitfields).

`message` is the error message.

`file`, `line` and `char` point to the original source location of the `BITSTRING`
constructor that failed.

## Bitstring comparison

```ocaml
val compare : bitstring -> bitstring -> int
```

`compare bs1 bs2` compares two bitstrings and returns zero if they are equal, a
negative number if bs1 < bs2, or a positive number if bs1 > bs2.  This tests
"semantic equality" which is not affected by the offset or alignment of the
underlying representation (see `Bitstring.bitstring`).

The ordering is total and lexicographic.

```ocaml
val equals : bitstring -> bitstring -> bool
```

`equals` returns true if and only if the two bitstrings are semantically equal.
It is the same as calling compare and testing if the result is 0, but usually more efficient.

## Bitstring manipulation

```ocaml
val bitstring_length : bitstring -> int
```

`bitstring_length bitstring` returns the length of the bitstring in bits.
Note this just returns the third field in the `Bitstring.bitstring` tuple.

```ocaml
val subbitstring : bitstring -> int -> int -> bitstring
```

`subbitstring bits off len` returns a sub-bitstring of the bitstring, starting
at offset off bits and with length len bits.  If the original bitstring is not
long enough to do this then the function raises `Invalid_argument "subbitstring"`.

Note that this function just changes the offset and length fields of the
`Bitstring.bitstring` tuple, so is very efficient.

```ocaml
val dropbits : int -> bitstring -> bitstring
```

Drop the first n bits of the bitstring and return a new bitstring which is
shorter by n bits.  If the length of the original bitstring is less than n
bits, this raises `Invalid_argument "dropbits"`.

Note that this function just changes the offset and length fields of the
`Bitstring.bitstring` tuple, so is very efficient.

```ocaml
val takebits : int -> bitstring -> bitstring
```

Take the first n bits of the bitstring and return a new bitstring which is
exactly n bits long.  If the length of the original bitstring is less than n
bits, this raises `Invalid_argument "takebits"`.

Note that this function just changes the offset and length fields of the
`Bitstring.bitstring` tuple, so is very efficient.

```ocaml
val concat : bitstring list -> bitstring
```

Concatenate a list of bitstrings together into a single bitstring.

## Constructing bitstrings

```ocaml
val empty_bitstring : bitstring
```

`empty_bitstring` is the empty, zero-length bitstring.

```ocaml
val create_bitstring : int -> bitstring
```

`create_bitstring n` creates an n bit bitstring containing all zeroes.

```ocaml
val make_bitstring : int -> char -> bitstring
```

make_bitstring n c creates an n bit bitstring containing the repeated 8 bit
pattern in c.  For example, `make_bitstring 16 '\x5a' `will create the bitstring
`0x5a5a` or in binary `0101 1010 0101 1010`.

Note that the length is in bits, not bytes. The length does NOT need to be a multiple of 8.

```ocaml
val zeroes_bitstring : int -> bitstring
```

zeroes_bitstring creates an n bit bitstring of all 0's. Actually this is the
same as `Bitstring.create_bitstring`.

```ocaml
val ones_bitstring : int -> bitstring
```

`ones_bitstring` creates an n bit bitstring of all 1's.

```ocaml
val bitstring_of_string : string -> bitstring
```

`bitstring_of_string str `creates a bitstring of length String.length str * 8
(bits) containing the bits in str.

Note that the bitstring uses str as the underlying string (see the
representation of Bitstring.bitstring) so you should not change str after
calling this.

```ocaml
val bitstring_of_file : string -> bitstring
```

bitstring_of_file filename loads the named file into a bitstring.

```ocaml
val bitstring_of_chan : Pervasives.in_channel -> bitstring
```

`bitstring_of_chan chan `loads the contents of the input channel chan as a
bitstring. The length of the final bitstring is determined by the remaining
input in chan, but will always be a multiple of 8 bits.

See also Bitstring.bitstring_of_chan_max.

```ocaml
val bitstring_of_chan_max : Pervasives.in_channel -> int -> bitstring
```

`bitstring_of_chan_max chan max` works like `Bitstring.bitstring_of_chan` but will
only read up to max bytes from the channel (or fewer if the end of input occurs
before that).

```ocaml
val bitstring_of_file_descr : Unix.file_descr -> bitstring
```

`bitstring_of_file_descr fd` loads the contents of the file descriptor fd as a
bitstring.  See also `Bitstring.bitstring_of_chan`,
`Bitstring.bitstring_of_file_descr_max`.

```ocaml
val bitstring_of_file_descr_max : Unix.file_descr -> int -> bitstring
```

`bitstring_of_file_descr_max fd max` works like `Bitstring.bitstring_of_file_descr`
but will only read up to max bytes from the channel (or fewer if the end of
input occurs before that).

## Converting bitstrings

```ocaml
val string_of_bitstring : bitstring -> string
```

`string_of_bitstring bitstring` converts a bitstring to a string (eg. to allow
comparison). This function is inefficient. In the best case when the bitstring
is nicely byte-aligned we do a `String.sub` operation. If the bitstring isn't
aligned then this involves a lot of bit twiddling and is particularly
inefficient.

If the bitstring is not a multiple of 8 bits wide then the final byte of the
string contains the high bits set to the remaining bits and the low bits set to
0.

```ocaml
val bitstring_to_file : bitstring -> string -> unit
```

`bitstring_to_file bits` filename writes the bitstring bits to the file
filename. It overwrites the output file. Some restrictions apply, see
`Bitstring.bitstring_to_chan`.

```ocaml
val bitstring_to_chan : bitstring -> Pervasives.out_channel -> unit
```

`bitstring_to_file bits` filename writes the bitstring bits to the channel chan.
Channels are made up of bytes, bitstrings can be any bit length including
fractions of bytes. So this function only works if the length of the bitstring
is an exact multiple of 8 bits (otherwise it raises `Invalid_argument "bitstring_to_chan"`).

Furthermore the function is efficient only in the case where the bitstring is
stored fully aligned, otherwise it has to do inefficient bit twiddling like
`Bitstring.string_of_bitstring`.

In the common case where the bitstring was generated by the `BITSTRING` operator
and is an exact multiple of 8 bits wide, then this function will always work
efficiently.

## Printing bitstrings

```ocaml
val hexdump_bitstring : Pervasives.out_channel -> bitstring -> unit
```

`hexdump_bitstring chan` bitstring prints the bitstring to the output channel in
a format similar to the Unix command `hexdump -C`.

## Bitstring buffer

```ocaml
module Buffer: sig .. end
```

Buffers are mainly used by the `BITSTRING` constructor, but may also be useful
for end users.

## Get/set bits

These functions let you manipulate individual bits in the bitstring. However
they are not particularly efficient and you should generally use the `bitmatch`
and `BITSTRING` operators when building and parsing bitstrings.

These functions all raise `Invalid_argument "index out of bounds"` if the index
is out of range of the bitstring.

```ocaml
val set : bitstring -> int -> unit
```

set bits n sets the nth bit in the bitstring to 1.

```ocaml
val clear : bitstring -> int -> unit
```

clear bits n sets the nth bit in the bitstring to 0.

```ocaml
val is_set : bitstring -> int -> bool
```

is_set bits n is true if the nth bit is set to 1.

```ocaml
val is_clear : bitstring -> int -> bool
```

is_clear bits n is true if the nth bit is set to 0.

```ocaml
val put : bitstring -> int -> int -> unit
```

put bits n v sets the nth bit in the bitstring to 1 if v is not zero, or to 0
if v is zero.

```ocaml
val get : bitstring -> int -> int
```

get bits n returns the nth bit (returns non-zero or 0).
