# InlineStrings

[![CI](https://github.com/JuliaData/InlineStrings.jl/workflows/CI/badge.svg)](https://github.com/JuliaData/InlineStrings.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/JuliaData/InlineStrings.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaData/InlineStrings.jl)
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

The package is tested against the latest Julia `1.x` release, the long-term support release `1.6`, and `nightly` on Linux, OS X, and Windows.

## Contributing and Questions

Contributions are very welcome, as are feature requests and suggestions. Please open an
[issue][issues-url] if you encounter any problems or would just like to ask a question.

[codecov-img]: https://codecov.io/gh/JuliaData/InlineStrings.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaData/InlineStrings.jl

[issues-url]: https://github.com/JuliaData/InlineStrings.jl/issues

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

An inline string is value equal (`==`), but not identical equal (`===`) to the corresponding `String`:

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

The hash value of an inline string is the same as it's `String` counterpart. Continuing the above example:

```julia-repl
julia> Dict(s => 1, i => 2)
Dict{AbstractString, Int64} with 1 entry:
  String15("abc") => 2

julia> y[i], y[s]
(2, 2)

julia> Dict(i => 1, i => 2)
Dict{String15, Int64} with 1 entry:
  "abc" => 2

```

Note how the `Dict` is passed two key-value pairs but the result contains only one. 
This is because we've only given the dictionary a single key (`Dict`s use the object's hash as it's underlying key), since `i` and `s` implement the same hash:

```julia-repl
julia> hash(i)
0x7690a66f302b3f70

julia> hash(s)
0x7690a66f302b3f70
```

#### Additional details

Use the REPL's help mode to see more details, such as those desribed here for `String15`:

```julia-repl
help?> String15

  String15(str::AbstractString)
  String15(bytes::AbstractVector{UInt8}, pos, len)
  String15(ptr::Ptr{UInt8}, [len])


  Custom fixed-size string with a fixed size of 16 bytes. 1 byte is used to store the length of the string. If an inline
  string is shorter than 15 bytes, the entire string still occupies the full 16 bytes since they are, by definition,
  fixed size. Otherwise, they can be treated just like normal String values. Note that sizeof(x) will return the # of
  codeunits in an String15 like String, not the total fixed size. For the fixed size, call sizeof(String15). String15 can
  be constructed from an existing String (String15(x::AbstractString)), from a byte buffer with position and length
  (String15(buf, pos, len)), from a pointer with optional length (String15(ptr, len)) or built iteratively by starting
  with x = String15() and calling x, overflowed = InlineStrings.addcodeunit(x, b::UInt8) which returns a new String15
  with the new codeunit b appended and an overflowed Bool value indicating whether too many codeunits have been appended
  for the fixed size. When constructed from a pointer, note that the ptr must point to valid memory or program data may
  become corrupt. If the len argument is specified with the pointer, it must fit within the fixed size of String15; if no
  length is provided, the C-string is assumed to be NUL-terminated. If the NUL-terminated string ends up longer than can
  fit in String15, an ArgumentError will be thrown.
```
