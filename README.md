# InlineStrings

[![CI](https://github.com/JuliaStrings/InlineStrings.jl/workflows/CI/badge.svg)](https://github.com/JuliaStrings/InlineStrings.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/JuliaStrings/InlineStrings.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/JuliaStrings/InlineStrings.jl)
[![deps](https://juliahub.com/docs/InlineStrings/deps.svg)](https://juliahub.com/ui/Packages/InlineStrings/muGbw?t=2)
[![version](https://juliahub.com/docs/InlineStrings/version.svg)](https://juliahub.com/ui/Packages/InlineStrings/muGbw)
[![pkgeval](https://juliahub.com/docs/InlineStrings/pkgeval.svg)](https://juliahub.com/ui/Packages/InlineStrings/muGbw)

*Fixed-width string types for facilitating certain string workflows in Julia*

## Installation

The package is registered in the General registry and so can be installed with `Pkg.add`.

```julia
julia> using Pkg; Pkg.add("InlineStrings")
```

## Project Status

The package tests the latest Julia `1.x` release, Julia `1.10`, and pre-releases
on Linux and Windows. It also tests the latest Julia release on macOS.

## Contributing and Questions

Contributions are very welcome, as are feature requests and suggestions. Please open an
[issue][issues-url] if you encounter any problems or would just like to ask a question.

[codecov-img]: https://codecov.io/gh/JuliaStrings/InlineStrings.jl/branch/main/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaStrings/InlineStrings.jl

[issues-url]: https://github.com/JuliaStrings/InlineStrings.jl/issues

## Usage

### `InlineString`

A set of custom string types of various fixed sizes. Each inline string is a
custom primitive type and can benefit from being stack friendly by avoiding
allocations/heap tracking in the GC. When used in an array, the elements are
able to be stored inline since each one has a fixed size. Currently support
inline strings from 1 byte up to 255 bytes.

The following types are supported: `String1`, `String3`, `String7`, `String15`,
`String31`, `String63`, `String127`, `String255`.

InlineStrings can be constructed by passing an `AbstractString`, byte vector
(`AbstractVector{UInt8}`) and position and length, or a pointer (`Ptr{UInt8}`)
and length. See the docstring for any individual InlineString type for more details
(like `?String3`).

To generically turn any string into the smallest possible InlineString type,
call `InlineString(x)`. To convert any iterator/array of strings to a single,
promoted `InlineString` type, the `inlinestrings(A)` utility function is exported.

#### Equality

An inline string is value equal (`==`), but not identical equal (`===`), to the corresponding `String`:

```julia-repl

julia> i = String15("abc")
"abc"

julia> s = "abc"
"abc"

julia> i == s
true

julia> i === s
false
```

#### Dictionaries & Hashes

The hash value of an inline string is the same as its `String` counterpart. Continuing the above example:

```julia-repl
julia> y = Dict(s => 1, i => 2)
Dict{AbstractString, Int64} with 1 entry:
  String15("abc") => 2

julia> y[i], y[s]
(2, 2)

julia> Dict(i => 1, i => 2)
Dict{String15, Int64} with 1 entry:
  "abc" => 2

```

The `Dict` receives two key-value pairs, but the result contains one entry.
This is because `Dict` uses both `hash` and `isequal`, and `i` and `s` have the
same value and hash:

```julia-repl
julia> hash(i)
0x7690a66f302b3f70

julia> hash(s)
0x7690a66f302b3f70
```

#### C compatibility

The codeunits of an inline string are stored in memory in the same byte order as a
`String`, followed by zero padding and a final byte holding the remaining capacity,
which doubles as the NUL terminator when the string is full. An inline string can
therefore be passed directly to C functions expecting a NUL-terminated string, just
like a `String`, with no allocation:

```julia-repl
julia> s = String15("hello")
"hello"

julia> @ccall strlen(s::Cstring)::Csize_t
0x0000000000000005
```

Note that an inline string is a plain value with no stable memory address, so
`pointer(s)` is not defined; `ccall` handles the conversion via `Base.cconvert`
(which returns a `Ref`), and `codeunits(s)` returns a non-`DenseVector` view for
the same reason.

#### Sorting

`String1`, `String3`, `String7` and `String15` plug into Base's radix sort
(`sort`, `sort!`, `sortperm`, with `rev`, `by` and `missing` all supported), so
sorting a `Vector{String7}` is several times faster than sorting the equivalent
`Vector{String}`. The wider types compare with a couple of integer instructions.

#### Additional details

Use the REPL's help mode to see more details, such as those described here for `String15`:

```julia-repl
help?> String15

  String15(str::AbstractString)
  String15(bytes::AbstractVector{UInt8}, pos, len)
  String15(ptr::Ptr{UInt8}, [len])


  Custom fixed-size string with a fixed size of 16 bytes, holding up to 15 codeunits. The codeunits are stored in
  memory in the same order as a String, followed by zero padding, with the final byte holding the remaining capacity;
  that byte doubles as a NUL terminator when the string is full, so an inline string can be passed directly to C
  functions expecting a NUL-terminated string (e.g. as a Cstring or Ptr{UInt8} ccall argument). If an inline string is
  shorter than 15 bytes, the entire string still occupies the full 16 bytes since they are, by definition, fixed size.
  Otherwise, they can be treated just like normal String values. Note that sizeof(x) will return the # of codeunits in
  an String15 like String, not the total fixed size. For the fixed size, call sizeof(String15). String15 can be
  constructed from an existing String (String15(x::AbstractString)), from a byte buffer with position and length
  (String15(buf, pos, len)), from a pointer with optional length (String15(ptr, len)) or built iteratively by starting
  with x = String15() and calling x, overflowed = InlineStrings.addcodeunit(x, b::UInt8) which returns a new String15
  with the new codeunit b appended and an overflowed Bool value indicating whether b did not fit (in which case x is
  returned unchanged). When constructed from a pointer, note that the ptr must point to valid memory or program data
  may become corrupt. If the len argument is specified with the pointer, it must fit within the fixed size of String15;
  if no length is provided, the C-string is assumed to be NUL-terminated. If the NUL-terminated string ends up longer
  than can fit in String15, an ArgumentError will be thrown.
```
