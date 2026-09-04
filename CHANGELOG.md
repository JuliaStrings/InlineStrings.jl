# Changelog

## 2.0.0

### Breaking changes

- **New in-memory layout.** The codeunits of an inline string are now stored in
  the same byte order as a `String`, followed by zero padding, with the *last*
  byte holding the remaining capacity (`N - 1 - length`) instead of the first
  byte holding the length. The capacity byte is `0` exactly when the string is
  full, so it doubles as a NUL terminator and inline strings can be passed
  straight to C (`@ccall strlen(s::Cstring)::Csize_t`). Anything that relied on
  the raw bits (`reinterpret`, data written with `write(io, x)` in 1.x, files
  serialized with `Serialization` in 1.x) is not compatible with 2.0.
- **`write(io, x::InlineString)` writes the codeunits**, exactly like `String`,
  and returns their count. In 1.x it wrote the full fixed-size bit pattern
  (e.g. 8 bytes for a `String7("ab")`). `read(io, ::Type{<:InlineString})` has
  been removed. Round-tripping through `Serialization` still works via a package
  extension.
- **`addcodeunit(x, b)` returns `x` unchanged** (with `overflowed = true`) when
  `b` does not fit; previously it returned a corrupted value.
- **`codeunits(x)` returns `InlineStrings.InlineCodeUnits`**, an
  `AbstractVector{UInt8}` that is not a `DenseVector`, and `pointer(x)` throws
  an `ArgumentError`. An inline string is a value with no stable address, so
  Base's pointer-based fast paths produced dangling pointers
  (`findfirst(==(0x0a), codeunits(s))`, `readline(IOBuffer(codeunits(s)))`).
- **`map(InlineString, A)` / `collect(InlineString, A)` are restricted to
  `Array` inputs** to avoid method ambiguities with LinearAlgebra;
  `InlineString.(A)` works for any array.
- The custom `InlineStringSort` radix sort and `Base.Sort.defalg` overload were
  removed; the small types plug into Base's own radix sort instead.
- Minimum supported Julia version is 1.10. `Parsers` is now a weak dependency
  (the extension still loads automatically when `Parsers` is loaded).

### Fixes

- Method ambiguities reported by Aqua (#64); Aqua now runs in CI.
- Dangling pointers from `codeunits` (#90).
- Matrix display alignment for `Any` matrices containing inline strings (#89).
- `isascii` ignored the last 1-3 bytes of a string.
- `reverse` corrupted strings containing 2- and 3-byte characters.
- `cmp`/`isless`/`==` between `String` and inline strings now compare
  codeunits (like `String` vs `String`) rather than characters, so malformed
  UTF-8 orders consistently.
- `isvalid(s, i)` returns `false` out of bounds instead of throwing.
- `T(buf, pos, len)` validates `pos`/`len` against the buffer bounds.
- Constructing from an `AbstractString` whose codeunits are not `UInt8`
  transcodes to UTF-8.
- `inlinestrings` measures non-`UInt8` inputs after UTF-8 transcoding and keeps
  working after the result widens to `String`.
- `chopprefix` and `chopsuffix` use the inline string's byte boundaries when
  the matched string uses non-`UInt8` code units.
- Multi-codeunit `findnext` calls validate out-of-bounds start indices.
- A `map` callback runs once per input character when its result must widen to
  `String`.
- `InlineString.(A)` and `map(InlineString, A)` preserve the shape of a matrix.

### Additions

- `map(f, s)`, `filter(f, s)`, and hence `uppercase`/`lowercase`, keep the inline
  string type (`map` falls back to `String` only when the result no longer fits) (#5).
- `startswith`/`endswith`/`==`/`cmp`/`isless` between inline strings of
  different sizes, and between inline strings and `SubString{String}`, have
  fast paths.
- `occursin`, `findfirst`/`findnext`/`findall` with string or character
  needles, and everything built on them (`replace`, `split`, ...) use a byte
  search over the inline value instead of Base's allocating character-based
  fallback.
- Byte access on the 32-byte and wider types is up to an order of magnitude
  faster.
- Range indexing avoids repeated wide-value spills, and wide ranges use a byte
  copy instead of large dynamic integer shifts.
- Searches with long needles use a bulk byte comparison after a short scalar
  prefix check.
- Repeating an empty inline string takes constant time.
