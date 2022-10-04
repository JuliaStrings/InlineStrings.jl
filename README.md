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
