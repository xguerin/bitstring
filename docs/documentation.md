# Guide

## Loading, creating bitstrings

The basic data type is the `Bitstring.bitstring`, a string of bits of arbitrary
length. Bitstrings can be any length in bits and operations do not need to be
byte-aligned (although they will generally be more efficient if they are
byte-aligned).

Internally a bitstring is stored as a normal OCaml `string` together with an
offset and length, where the offset and length are measured in bits. Thus one
can efficiently form substrings of bitstrings, overlay a bitstring on existing
data, and load and save bitstrings from files or other external sources.

To load a bitstring from a file use `Bitstring.bitstring_of_file` or
`Bitstring.bitstring_of_chan`. There are also functions to create bitstrings
from arbitrary data. See the [reference](/reference) section.

## Matching bitstrings with patterns

Use the `bitstring` extension of the `match` operator (part of the syntax extension)
to break apart a bitstring into its fields. `match%bitstring` works a lot like the
OCaml `match` operator. Please note the use of the `{| ... |}` verbatim notation
for the matching patterns.

The general form of `match%bitstring` is:

```ocaml
match%bitstring EXPRESSION with

| {| PATTERN |} -> CODE
| {| PATTERN |} -> CODE
|  ...
```

As with normal match, the statement attempts to match the bitstring against each
pattern in turn. If none of the patterns match then the standard library
`Match_failure` exception is thrown.

Patterns look a bit different from normal match patterns. They consist of a list
of bitfields separated by `;` where each bitfield contains a bind variable, the
width (in bits) of the field, and other information. Some example patterns:

```ocaml
match%bitstring bits with

| {| version : 8; name : 8; param : 8 |} -> ...

   (* Bitstring of at least 3 bytes. First byte is the version
      number, second byte is a field called name, third byte is
      a field called parameter. *)

| {| flag : 1 |} ->
   printf "flag is %b\n" flag

   (* A single flag bit (mapped into an OCaml boolean). *)

| {| len : 4; data : 1 + len |} ->
   printf "len = %d, data = 0x%Lx\n" len data

   (* A 4-bit length, followed by 1-16 bits of data, where the
      length of the data is computed from len. *)

| {| ipv6_source : 128 : bitstring;
     ipv6_dest   : 128 : bitstring |} -> ...

   (* IPv6 source and destination addresses. Each is 128 bits
      and is mapped into a bitstring type which will be a substring
      of the main bitstring expression. *)
```

You can also add conditional when-clauses:

```ocaml
| {| version : 4 |}
    when version = 4 || version = 6 -> ...

   (* Only match and run the code when version is 4 or 6. If
      it isn't we will drop through to the next case. *)
```

Note that the pattern is only compared against the first part of the bitstring
(there may be more data in the bitstring following the pattern, which is not
matched). In terms of regular expressions you might say that the pattern
matches `^pattern`, not `^pattern$`. To ensure that the bitstring contains only
the pattern, add a length -1 bitstring to the end and test that its length is
zero in the when-clause:

```ocaml
| {| n : 4;
     rest : -1 : bitstring |}
     when Bitstring.bitstring_length rest = 0 -> ...

   (* Only matches exactly 4 bits. *)
```

Normally the first part of each field is a binding variable,
but you can also match a constant, as in:

```ocaml
| {| (4|6) : 4 |} -> ...

   (* Only matches if the first 4 bits contain either the integer 4 or the integer 6. *)
```

One may also match on strings:

```ocaml
| {| "MAGIC" : 5*8 : string |} -> ...

   (* Only matches if the string "MAGIC" appears at the start of the input. *)
```

### Pattern field reference

The exact format of each pattern field is:

`pattern : length [: qualifier [,qualifier ...]]`

`pattern` is the pattern, binding variable name, or constant to match. `length`
is the length in bits which may be either a constant or an expression. The
length expression is just an OCaml expression and can use any values defined in
the program, and refer back to earlier fields (but not to later fields).

Integers can only have lengths in the range `[1..64]` bits. See the [integer
types](#integer-types) section below for how these are mapped to the OCaml
`int`/`int32`/`int64` types. This is checked at compile time if the length
expression is constant, otherwise it is checked at runtime and you will get a
runtime exception eg. in the case of a computed length expression.

A bitstring field of length `-1` matches all the rest of the bitstring (thus
this is only useful as the last field in a pattern).

A bitstring field of length `0` matches an empty bitstring (occasionally useful
when matching optional subfields).

Qualifiers are a list of identifiers/expressions which control the type,
signedness and endianness of the field. Permissible qualifiers are:

| Qualifier      | Description |
|:---------------|:------------|
| `int`          | field has an integer type |
| `string`       | field is a string type |
| `bitstring`    | field is a bitstring type |
| `signed`       | field is signed |
| `unsigned`     | field is unsigned |
| `bigendian`    | field is big endian - a.k.a network byte order |
| `littleendian` | field is little endian - a.k.a Intel byte order |
| `nativeendian` | field is same endianness as the machine |
| `endian(expr)` | `expr` should be an expression which evaluates to a `Bitstring.endian` type |
| `offset(expr)` | see [computed offsets](#computed-offsets) below |
| `check(expr)`  | apply some constraint to the field |
| `bind(expr)`   | bind the field to `expr` |
| `map(lambda)`  | apply `lambda` to the field |

`Bitstring.endian` is either `LittleEndian`, `BigEndian` or `NativeEndian`. The
expression in `endian(expr)` is an arbitrary OCaml expression and can use the
value of earlier fields in the bitmatch.

The default settings are `int`, `unsigned`, `bigendian`, no offset.

Note that many of these qualifiers cannot be used together, eg. bitstrings do
not have endianness. The syntax extension should give you a compile-time error
if you use incompatible qualifiers.

### Default match cases

As well as a list of fields, it is possible to name the bitstring and/or have a
default match case:

```ocaml
| {| _ |} -> ...

   (* Default match case. *)

| {| _ |} as pkt -> ...

   (* Default match case, with 'pkt' bound to the whole bitstring. *)
```

### Function definition

The `function` keyword can also be used for pattern matching:

```ocaml
let pattern_matcher = function%bitstring
| {| false : 1
   ; a : 2
   ; b : 16 : bigendian
   ; ...
   |} -> (* Do something *)
| {| _ |} -> (* Do something else *)
```

## Constructing bitstrings

Bitstrings may be constructed using the `bitstring` extension of the `let`
keyword. The `let%bitstring` expression takes a list of fields, similar to the
list of fields for matching:

```ocaml
let version = 1 ;;
let data = 10 ;;
let%bitstring bits = {|
  version : 4;
  data : 12
|} ;;

(* Constructs a 16-bit bitstring with the first four bits containing
   the integer 1, and the following 12 bits containing the integer 10,
   arranged in network byte order. *)

Bitstring.hexdump_bitstring stdout bits ;;

(* Prints:
   00000000  10 0a         |..              |
 *)
```

The format of each field is the same as for pattern fields (see [Pattern field
reference section](#pattern-field-reference)), and things like computed length
fields, fixed value fields, insertion of bitstrings within bitstrings, etc. are
all supported.

### Construction exception

The `let%bitstring` expression may throw a [Bitstring.Construct_failure](/reference/#exceptions)
exception at runtime.

Runtime errors include:

* `int` field length not in the range [1..64]
* a bitstring with a length declared which doesn't have the same length at runtime
* trying to insert an out-of-range value into an `int` field

## Integer types

Integer types are mapped to OCaml types `bool`, `int`, `int32` or `int64` using
a system which tries to ensure that (a) the types are reasonably predictable and
(b) the most efficient type is preferred.

The rules are slightly different depending on whether the bit length expression
in the field is a compile-time constant or a computed expression.

Detection of compile-time constants is quite simplistic so only simple integer
literals and simple expressions (eg. `5 * 8`) are recognized as constants.

In any case the bit size of an integer is limited to the range `[1..64]`. This
is detected as a compile-time error if that is possible, otherwise a runtime
check is added which can throw an `Invalid_argument` exception.

The mapping is thus:

| Bit size | Constant | Computed expression |
|:---------|:---------|:--------------------|
| 1        | `bool`   | `int64`             |
| 2..31    | `int`    | `int64`             |
| 32       | `int32`  | `int64`             |
| 33..64   | `int64`  | `int64`             |

A possible future extension may allow people with 64 bit computers to specify a
more optimal `int` type for bit sizes in the range `32..63`. If this was
implemented then such code _could not even be compiled_ on 32 bit platforms,
so it would limit portability.

Another future extension may be to allow computed expressions to assert min/max
range for the bit size, allowing a more efficient data type than `int64` to be
used. (Of course under such circumstances there would still need to be a
runtime check to enforce the size).

## Advanced pattern-matching

### Computed offsets

You can add an `offset(..)` qualifier to bitmatch patterns in order to move the
current offset within the bitstring forwards.

For example:

```ocaml
match%bitstring bits with
| {| field1 : 8;
     field2 : 8 : offset(160) |} -> ...
```

matches `field1` at the start of the bitstring and `field2` at 160 bits into the
bitstring. The middle 152 bits go unmatched (ie. can be anything).

The generated code is efficient. If field lengths and offsets are known to be
constant at compile time, then almost all runtime checks are avoided.
Non-constant field lengths and/or non-constant offsets can result in more
runtime checks being added.

Note that moving the offset backwards, and moving the offset in
`let%bitstring` expressions, are both not supported at present.

### Check expressions

You can add a `check(expr)` qualifier to bitmatch patterns. If the expression
evaluates to false then the current match case fails to match (in other words,
we fall through to the next match case - there is no error).

For example:

```ocaml
match%bitstring bits with
| { field : 16 : check (field > 100) } -> ...
```

Note the difference between a check expression and a when-clause is that the
when-clause is evaluated after all the fields have been matched. On the other
hand a check expression is evaluated after the individual field has been
matched, which means it is potentially more efficient (if the check expression
fails then we don't waste any time matching later fields).

We wanted to use the notation `when(expr)` here, but because `when` is a
reserved word we could not do this.

### Bind expressions

A bind expression is used to change the value of a matched field. For example:

```ocaml
match%bitstring bits with
| { len : 16 : bind (len * 8);
    field : len : bitstring } -> ...
```

In the example, after 'len' has been matched, its value would be multiplied by
8, so the width of 'field' is the matched value multiplied by 8.

In the general case:

```ocaml
| { field : ... : bind (expr) } -> ...
```

evaluates the following after the field has been matched:

```ocaml
let field = expr in
   (* remaining fields *)
```

### Map expressions

A map expression is used to apply a `lambda` expression to a matched field. The
matched field would then contain the result of the application:

```ocaml
{| field : size : map (fun v -> do_something_with v) }|
```

evaluates the following after the field has been matched:

```ocaml
let field = (fun v -> do_something_with v) temporary_parsed_field in
   (* remaining fields *)
```

### Order of evaluation

The choice is arbitrary, but we have chosen that check expressions are evaluated
first, and bind/map expressions are evaluated after.

This means that the result of `bind()` or `map()` is _not_ available in the check
expression.

Note that this rule applies regardless of the order of `check()`, `bind()`,
or `map()` in the source code.

### Saving bit offsets

Use `save_offset_to(variable)` to save the current bit offset within the match
to a variable (strictly speaking, to a pattern). This variable is then made
available in any `check()` and `bind()` clauses in the current field, _and_
to any later fields, and to the code after the `->`.

For example:

```ocaml
match%bitstring bits with
| {| len : 16;
     _ : len : bitstring;
     field : 16 : save_offset_to (field_offset) |} ->
      printf "field is at bit offset %d in the match\n" field_offset
```

(In that example, `field_offset` should always have the value `len+16`).

## Security and type safety

### Security on input

The main concerns for input are buffer overflows and denial of service.

It is believed that this library is robust against attempted buffer overflows.
In addition to OCaml's normal bounds checks, we check that field lengths are >=
0, and many additional checks.

Denial of service attacks are more problematic. We only work forwards through
the bitstring, thus computation will eventually terminate. As for computed
lengths, code such as this is thought to be secure:

```ocaml
match%bitstring bits with
| {| len : 64;
     buffer : Int64.to_int len : bitstring |} -> ...
```

The `len` field can be set arbitrarily large by an attacker, but when
pattern-matching against the `buffer` field this merely causes a test such as
`if len <= remaining_size` to fail. Even if the length is chosen so that
`buffer` bitstring is allocated, the allocation of sub-bitstrings is efficient
and doesn't involve an arbitary-sized allocation or any copying.

However the above does not necessarily apply to strings used in matching, since
they may cause the library to use the
[Bitstring.string_of_bitstring](/reference/#converting-bitstrings) function,
which allocates a string. So you should take care if you use the `string` type
particularly with a computed length that is derived from external input.

The main protection against attackers should be to ensure that the main program
will only read input bitstrings up to a certain length, which is outside the
scope of this library.

### Security on output

As with the input side, computed lengths are believed to be safe. For example:

```ocaml
let len = read_untrusted_source () in
let buffer = allocate_bitstring () in
[%bitstring {|
  buffer : len : bitstring
|}]
```

This code merely causes a check that buffer's length is the same as `len`.
However the program function `allocate_bitstring` must refuse to allocate an
oversized buffer (but that is outside the scope of this library).

### Order of evaluation

In `match%bitstring` statements, fields are evaluated left to right.

Note that the when-clause is evaluated _last_, so if you are relying on the
when-clause to filter cases then your code may do a lot of extra and unncessary
pattern-matching work on fields which may never be needed just to evaluate the
when-clause. Either rearrange the code to do only the first part of the match,
followed by the when-clause, followed by a second inner bitmatch, or use a 
`check()` qualifier within fields.

### Safety

The current implementation is believed to be fully type-safe, and makes compile
and run-time checks where appropriate. If you find a case where a check is
missing please submit a bug report or a patch.

## Limits

These are thought to be the current limits:

* Integers: `[1..64]` bits.
* Bitstrings (32-bit): maximum length is limited by the string size, ie. 16 MBytes.
* Bitstrings (64-bit): maximum length is thought to be limited by the string size, ie. effectively unlimited.

Bitstrings must be loaded into memory before we can match against them. Thus
available memory may be considered a limit for some applications.
