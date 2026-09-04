module InlineStrings

export InlineString, InlineStringType, inlinestrings
export @inline_str

"""
    InlineString

A set of custom string types of various fixed sizes. Each inline string
is a custom primitive type and can benefit from being stack friendly by
avoiding allocations/heap tracking in the GC. When used in an array,
the elements are able to be stored inline since each one has a fixed
size. Currently support inline strings from 1 byte up to 255 bytes.
See more details by looking at individual docs for [`String1`](@ref),
[`String3`](@ref), [`String7`](@ref), [`String15`](@ref), [`String31`](@ref),
[`String63`](@ref), [`String127`](@ref), or [`String255`](@ref).
"""
abstract type InlineString <: AbstractString end

for sz in (2, 4, 8, 16, 32, 64, 128, 256)
    nm = Symbol(:String, sz-1)
    nma = Symbol(:InlineString, sz-1)
    macro_nm = Symbol(:inline, sz-1, :_str)
    at_macro_nm = Symbol("@", macro_nm)
    @eval begin
        """
            $($nm)(str::AbstractString)
            $($nm)(bytes::AbstractVector{UInt8}, pos, len)
            $($nm)(ptr::Ptr{UInt8}, [len])

        Custom fixed-size string with a fixed size of $($sz) bytes, holding up to
        $($(max(1, sz - 1))) codeunits. The codeunits are stored in memory in the same
        order as a `String`, followed by zero padding, with the final byte holding
        the remaining capacity; that byte doubles as a NUL terminator when the string
        is full, so an inline string can be passed directly to C functions expecting
        a NUL-terminated string (e.g. as a `Cstring` or `Ptr{UInt8}` `ccall` argument).
        If an inline string is shorter than $($(max(1, sz - 1))) bytes, the entire
        string still occupies the full $($sz) bytes since they are,
        by definition, fixed size. Otherwise, they can be treated
        just like normal `String` values. Note that `sizeof(x)` will
        return the # of _codeunits_ in an $($nm) like `String`, not
        the total fixed size. For the fixed size, call `sizeof($($nm))`.
        $($nm) can be constructed from an existing `String` (`$($nm)(x::AbstractString)`),
        from a byte buffer with position and length (`$($nm)(buf, pos, len)`),
        from a pointer with optional length (`$($nm)(ptr, len)`)
        or built iteratively by starting with `x = $($nm)()` and calling
        `x, overflowed = InlineStrings.addcodeunit(x, b::UInt8)` which returns a
        new $($nm) with the new codeunit `b` appended and an `overflowed` `Bool`
        value indicating whether `b` did not fit (in which case `x` is returned
        unchanged). When constructed from a pointer, note that the `ptr` must
        point to valid memory or program data may become corrupt. If the `len`
        argument is specified with the pointer, it must fit within the fixed size
        of $($nm); if no length is provided, the C-string is assumed to be
        NUL-terminated. If the NUL-terminated string ends up longer than can
        fit in $($nm), an ArgumentError will be thrown.
        """
        primitive type $nm <: InlineString $(sz * 8) end
        const $nma = $nm
        export $nm
        export $nma

        """
            inline$($(sz-1))"string"

        Macro to create a [`$($nm)`](@ref) with a fixed size of $($sz) bytes.
        """
        macro $macro_nm(ex)
            T = InlineStringType($(sz - 1))
            T(unescape_string(ex))
        end

        export $at_macro_nm
    end
end

const SmallInlineStrings = Union{String1, String3, String7, String15}

const InlineStringTypes = Union{InlineString1,
                            InlineString3,
                            InlineString7,
                            InlineString15,
                            InlineString31,
                            InlineString63,
                            InlineString127,
                            InlineString255}

function Base.promote_rule(::Type{T}, ::Type{S}) where {T <: InlineString, S <: InlineString}
    T === InlineString1 && return S
    S === InlineString1 && return T
    T === InlineString3 && return S
    S === InlineString3 && return T
    T === InlineString7 && return S
    S === InlineString7 && return T
    T === InlineString15 && return S
    S === InlineString15 && return T
    T === InlineString31 && return S
    S === InlineString31 && return T
    T === InlineString63 && return S
    S === InlineString63 && return T
    T === InlineString127 && return S
    S === InlineString127 && return T
    return InlineString255
end

Base.promote_rule(::Type{T}, ::Type{String}) where {T <: InlineString} = String

Base.widen(::Type{InlineString1}) = InlineString3
Base.widen(::Type{InlineString3}) = InlineString7
Base.widen(::Type{InlineString7}) = InlineString15
Base.widen(::Type{InlineString15}) = InlineString31
Base.widen(::Type{InlineString31}) = InlineString63
Base.widen(::Type{InlineString63}) = InlineString127
Base.widen(::Type{InlineString127}) = InlineString255
Base.widen(::Type{InlineString255}) = String

#-----------------------------------------------------------------------------
# Layout
#
# An inline string `T` with `N = sizeof(T)` bytes holding `len` codeunits
# (`len <= N - 1`) is laid out in memory exactly like a NUL-terminated C string:
#
#   byte:     | 1 ... len   | len+1 ... N-1 | N                    |
#   content:  | codeunits   | zero padding  | capacity = N - 1 - len |
#
# Byte 1 is the least significant byte of the primitive value, so the codeunits
# are in `String` order in memory and `Ref(x)` can be handed straight to C. The
# capacity byte is 0 exactly when the string is full, doubling as the NUL
# terminator. All unused bytes are always zero so that `==` is a plain integer
# comparison (`Base.eq_int`).
#
# Shifts by the full bit width are undefined, so every helper below only ever
# shifts by `8 * k` with `1 <= k <= N - 1` (or short-circuits).
#-----------------------------------------------------------------------------

@inline _zero(::Type{T}) where {T <: InlineString} = Base.zext_int(T, 0x00)
@inline _capmask(::Type{T}) where {T <: InlineString} =
    Base.shl_int(Base.zext_int(T, 0xff), 8 * (sizeof(T) - 1))
@inline _capbyte(x::T) where {T <: InlineString} =
    Base.trunc_int(UInt8, Base.lshr_int(x, 8 * (sizeof(T) - 1)))
# the codeunits with the capacity byte cleared
@inline _data(x::T) where {T <: InlineString} = Base.and_int(x, Base.not_int(_capmask(T)))
# set the capacity byte of `x` (which must be zero) for a string of `len` codeunits
@inline function _withlen(x::T, len::Int) where {T <: InlineString}
    cap = Base.zext_int(T, (sizeof(T) - 1 - len) % UInt8)
    return Base.or_int(x, Base.shl_int(cap, 8 * (sizeof(T) - 1)))
end
# keep the lowest `n` bytes (0 <= n <= N - 1), zeroing everything above them
@inline function _keeplow(x::T, n::Int) where {T <: InlineString}
    n == 0 && return _zero(T)
    sh = 8 * (sizeof(T) - n)
    return Base.lshr_int(Base.shl_int(x, sh), sh)
end
# the inline string made of codeunits `i` through `j` of `s` (`j < i` gives "")
@inline function _sub(s::T, i::Int, j::Int) where {T <: InlineString}
    n = j - i + 1
    n <= 0 && return T()
    if sizeof(T) >= 128
        ref = Ref(s)
        return GC.@preserve ref _load(T, _ptr(ref) + (i - 1), n, n)
    end
    sh = 8 * (sizeof(T) - j)  # shift out everything above byte j, then down to byte i
    return _withlen(Base.lshr_int(Base.shl_int(s, sh), sh + 8 * (i - 1)), n)
end

Base.ncodeunits(x::T) where {T <: InlineString} = (sizeof(T) - 1) - Int(_capbyte(x))
Base.codeunit(::InlineString) = UInt8

# Random byte access. Dynamic shifts of a 256+ bit integer are expensive, so
# the wider types go through a byte tuple instead (a stack spill and a load).
@inline _bytes(x::T) where {T <: InlineString} = reinterpret(NTuple{sizeof(T), UInt8}, x)
const _SHIFTMAX = 16
@inline function _byte(x::T, i::Int) where {T <: InlineString}
    if sizeof(T) <= _SHIFTMAX
        return Base.trunc_int(UInt8, Base.lshr_int(x, 8 * (i - 1)))
    else
        return @inbounds _bytes(x)[i]
    end
end

Base.@propagate_inbounds function Base.codeunit(x::InlineString, i::Int)
    @boundscheck checkbounds(Bool, x, i) || throw(BoundsError(x, i))
    return _byte(x, i)
end

"""
    InlineStrings.InlineCodeUnits

The `AbstractVector{UInt8}` returned by `codeunits(::InlineString)`. Unlike
`Base.CodeUnits`, it is not a `DenseVector`: an inline string is a plain value
with no stable memory address, so no `pointer` can be taken to it.
"""
struct InlineCodeUnits{T <: InlineString} <: AbstractVector{UInt8}
    s::T
end
Base.codeunits(s::InlineString) = InlineCodeUnits(s)
Base.size(c::InlineCodeUnits) = (ncodeunits(c.s),)
Base.sizeof(c::InlineCodeUnits) = ncodeunits(c.s)
Base.IndexStyle(::Type{<:InlineCodeUnits}) = IndexLinear()
Base.@propagate_inbounds Base.getindex(c::InlineCodeUnits, i::Int) = codeunit(c.s, i)
Base.write(io::IO, c::InlineCodeUnits) = write(io, c.s)
Base.String(c::InlineCodeUnits) = String(c.s)
Base.Vector{UInt8}(c::InlineCodeUnits) = Vector{UInt8}(c.s)

Base.pointer(::T) where {T <: InlineString} = throw(ArgumentError(
    "$T values have no stable memory address, so `pointer` is not defined; " *
    "pass the string to `ccall` directly (as `Cstring` or `Ptr{UInt8}`), or use `String(x)`"))

# `ref = Ref(x)` inside a `GC.@preserve ref` block gives a (stack-allocated)
# pointer to the bytes of `x`, laid out like a C string.
@inline _ptr(ref::Base.RefValue) = Ptr{UInt8}(pointer_from_objref(ref))

# widest type loaded whole (then masked) rather than memcpy'd into a zeroed Ref
const _LOADMAX = 32
# Build a `T` from `len` bytes at `ptr`, where `avail` bytes are readable at `ptr`.
@inline function _load(::Type{T}, ptr::Ptr{UInt8}, len::Int, avail::Int) where {T <: InlineString}
    if sizeof(T) <= _LOADMAX && avail >= sizeof(T)
        return _withlen(_keeplow(unsafe_load(Ptr{T}(ptr)), len), len)
    else
        ref = Ref{T}(_zero(T))
        GC.@preserve ref unsafe_copyto!(Ptr{UInt8}(pointer_from_objref(ref)), ptr, len)
        return _withlen(ref[], len)
    end
end

# Build a `T` of `len` codeunits, reading codeunit `i` as `getbyte(i)`.
function _frombytes(::Type{T}, getbyte::F, len::Int) where {T <: InlineString, F}
    ref = Ref{T}(_zero(T))
    GC.@preserve ref begin
        ptr = Ptr{UInt8}(pointer_from_objref(ref))
        for i = 1:len
            unsafe_store!(ptr, getbyte(i)::UInt8, i)
        end
    end
    return _withlen(ref[], len)
end

# byte vectors we can take a pointer into
const PointerBytes = Union{DenseVector{UInt8}, Base.FastContiguousSubArray{UInt8, 1, <:DenseVector{UInt8}}}

function Base.String(x::InlineString)
    ref = Ref(x)
    return GC.@preserve ref unsafe_string(_ptr(ref), ncodeunits(x))
end

Base.Symbol(x::T) where {T <: InlineString} =
    ccall(:jl_symbol_n, Ref{Symbol}, (Ref{T}, Int), Ref(x), sizeof(x))

Base.cconvert(::Type{Ptr{UInt8}}, x::InlineString) = Ref(x)
Base.cconvert(::Type{Ptr{Int8}}, x::InlineString) = Ref(x)
function Base.cconvert(::Type{Cstring}, x::InlineString)
    ref = Ref(x)
    GC.@preserve ref Base.containsnul(Ptr{Int8}(pointer_from_objref(ref)), sizeof(x)) &&
        throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(x))"))
    return ref
end
Base.unsafe_convert(::Type{Ptr{UInt8}}, r::Base.RefValue{<:InlineString}) = Ptr{UInt8}(pointer_from_objref(r))
Base.unsafe_convert(::Type{Ptr{Int8}}, r::Base.RefValue{<:InlineString}) = Ptr{Int8}(pointer_from_objref(r))
Base.unsafe_convert(::Type{Cstring}, r::Base.RefValue{<:InlineString}) = Cstring(Ptr{Cchar}(pointer_from_objref(r)))

function Base.Vector{UInt8}(x::InlineString)
    n = ncodeunits(x)
    out = Vector{UInt8}(undef, n)
    ref = Ref(x)
    GC.@preserve ref out unsafe_copyto!(pointer(out), _ptr(ref), n)
    return out
end
Base.Array{UInt8}(x::InlineString) = Vector{UInt8}(x)

Base.show(io::IO, ::MIME"text/plain", s::InlineString) = Base.print_quoted(io, s)
function Base.show(io::IO, s::InlineString)  # So `repr` shows how to recreate `s`
    if get(io, :typeinfo, Any) === typeof(s)
        Base.print_quoted(io, s)
    else
        print(io, typeof(s), "(")
        Base.print_quoted(io, s)
        print(io, ")")
    end
end
# Matrices are printed with the 3-arg `show` but aligned with the 2-arg one by
# default; measure what actually gets printed.
Base.alignment(io::IO, s::InlineString) =
    (0, textwidth(sprint(show, MIME("text/plain"), s; context=io, sizehint=0)))

"""
    InlineStrings.addcodeunit(x::InlineString, b::UInt8) -> (x′, overflowed::Bool)

Append the codeunit `b` to `x`. If `x` is already full, `x` is returned
unchanged and `overflowed` is `true`.
"""
function addcodeunit(x::T, b::UInt8) where {T <: InlineString}
    len = ncodeunits(x)
    len + 1 >= sizeof(T) && return x, true
    x = Base.or_int(x, Base.shl_int(Base.zext_int(T, b), 8 * len))
    # the capacity byte is >= 1 here, so decrementing it can't borrow into the data
    return Base.sub_int(x, Base.shl_int(Base.zext_int(T, 0x01), 8 * (sizeof(T) - 1))), false
end

for T in (:InlineString1, :InlineString3, :InlineString7, :InlineString15, :InlineString31, :InlineString63, :InlineString127, :InlineString255)
    @eval $T() = _withlen(_zero($T), 0)

    @eval function $T(x::AbstractString)
        codeunit(x) === UInt8 || return $T(String(x))
        len = ncodeunits(x)
        len < sizeof($T) || stringtoolong($T, len)
        return _frombytes($T, i -> @inbounds(codeunit(x, i)), len)
    end

    # A `String` is always allocated with at least 8 readable bytes of data.
    @eval function $T(x::String)
        len = sizeof(x)
        len < sizeof($T) || stringtoolong($T, len)
        return GC.@preserve x _load($T, pointer(x), len, 8)
    end

    @eval function $T(x::SubString{String})
        len = sizeof(x)
        len < sizeof($T) || stringtoolong($T, len)
        # readable bytes: the rest of the parent (incl. its NUL), at least 8 from its start
        avail = max(sizeof(x.string) + 1, 8) - x.offset
        return GC.@preserve x _load($T, pointer(x), len, avail)
    end

    @eval function $T(x::SubString{S}) where {S <: InlineString}
        return $T(_sub(x.string, x.offset + 1, x.offset + x.ncodeunits))
    end

    @eval function $T(buf::AbstractVector{UInt8}, pos::Integer=firstindex(buf), len::Integer=lastindex(buf) - pos + 1)
        pos = Int(pos)
        len = Int(len)
        len < 0 && invalidlength(len)
        len < sizeof($T) || stringtoolong($T, len)
        len == 0 && return $T()
        !(firstindex(buf) <= pos <= lastindex(buf) && len - 1 <= lastindex(buf) - pos) && buftoosmall(len)
        if buf isa PointerBytes
            return GC.@preserve buf _load($T, pointer(buf, pos), len, lastindex(buf) - pos + 1)
        else
            return _frombytes($T, i -> @inbounds(buf[pos + i - 1]), len)
        end
    end

    @eval function $T(ptr::Ptr{UInt8}, len::Union{Nothing, Integer}=nothing)
        ptr == C_NULL && nullptr($T)
        if len === nothing
            n = Int(ccall(:strlen, Csize_t, (Ptr{UInt8},), ptr))
        else
            n = Int(len)
            n < 0 && invalidlength(n)
        end
        n < sizeof($T) || stringtoolong($T, n)
        return _load($T, ptr, n, n)
    end

    # between InlineStringTypes
    @eval function $T(x::S) where {S <: InlineString}
        $T === S && return x
        len = ncodeunits(x)
        if sizeof($T) < sizeof(S)
            len < sizeof($T) || stringtoolong($T, len)
            return _withlen(Base.trunc_int($T, _data(x)), len)
        else
            return _withlen(Base.zext_int($T, _data(x)), len)
        end
    end
end

@noinline nullptr(T) = throw(ArgumentError("cannot convert NULL to $T"))
@noinline buftoosmall(n) = throw(ArgumentError("input buffer too short for requested length: $n"))
@noinline invalidlength(n) = throw(ArgumentError("length must be nonnegative: $n"))
@noinline stringtoolong(T, n) = throw(ArgumentError("string too large ($n) to convert to $T"))

function InlineStringType(n::Integer)
    n < 0 && invalidlength(n)
    n > 255 && stringtoolong(InlineString, n)
    return n < 2   ? InlineString1   : n < 4  ? InlineString3  :
           n < 8   ? InlineString7   : n < 16 ? InlineString15 :
           n < 32  ? InlineString31  : n < 64 ? InlineString63 :
           n < 128 ? InlineString127 : InlineString255
end

InlineString(x::InlineString) = x
function InlineString(x::AbstractString)::InlineStringTypes
    codeunit(x) === UInt8 || return InlineString(String(x))
    return (InlineStringType(ncodeunits(x)))(x)
end

"""
    inline"string"

Macro to create an [`InlineString`](@ref).
"""
macro inline_str(ex)
    InlineString(unescape_string(ex))
end

#-----------------------------------------------------------------------------
# Equality, ordering, hashing
#-----------------------------------------------------------------------------

Base.:(==)(x::T, y::T) where {T <: InlineString} = Base.eq_int(x, y)
function Base.:(==)(x::InlineString, y::InlineString)
    T = promote_type(typeof(x), typeof(y))
    return T(x) === T(y)
end
function Base.:(==)(x::Union{String, SubString{String}}, y::T) where {T <: InlineString}
    len = sizeof(x)
    len == ncodeunits(y) || return false
    if sizeof(T) <= 16
        return GC.@preserve x _load(T, pointer(x), len, x isa String ? 8 : len) === y
    else
        ref = Ref(y)
        return GC.@preserve x ref _memcmp(pointer(x), _ptr(ref), len) == 0
    end
end
_memcmp(a::Ptr{UInt8}, b::Ptr{UInt8}, n::Int) =
    ccall(:memcmp, Cint, (Ptr{UInt8}, Ptr{UInt8}, Csize_t), a, b, n % Csize_t)
Base.:(==)(y::InlineString, x::Union{String, SubString{String}}) = x == y

# Sort key: byte-swapping puts the first codeunit in the most significant byte
# and the (flipped, so monotone in the length) capacity byte in the least
# significant one, so unsigned integer order of the keys is exactly the
# lexicographic codeunit order of the strings, shorter prefix first.
@inline _key(x::T) where {T <: InlineString} = Base.bswap_int(Base.xor_int(x, _capmask(T)))
@inline _unkey(k::T) where {T <: InlineString} = Base.xor_int(Base.bswap_int(k), _capmask(T))

# For the wide types, compare 64-bit words from the front and stop at the first
# difference instead of byte-swapping the whole value.
@generated function _cmpwords(a::T, b::T, ::Val{K}) where {T, K}
    ex = Expr(:block)
    for k in 1:K
        sh = 64 * (k - 1)
        push!(ex.args, quote
            wa = bswap(Base.trunc_int(UInt64, Base.lshr_int(a, $sh)))
            wb = bswap(Base.trunc_int(UInt64, Base.lshr_int(b, $sh)))
            wa == wb || return ifelse(wa < wb, -1, 1)
        end)
    end
    push!(ex.args, :(return 0))
    return ex
end
@inline function Base.cmp(a::T, b::T) where {T <: InlineString}
    if sizeof(T) <= 16
        return Base.eq_int(a, b) ? 0 : Base.ult_int(_key(a), _key(b)) ? -1 : 1
    else
        cm = _capmask(T)
        return _cmpwords(Base.xor_int(a, cm), Base.xor_int(b, cm), Val(sizeof(T) >> 3))
    end
end
Base.isless(a::T, b::T) where {T <: InlineString} = Base.ult_int(_key(a), _key(b))
function Base.isless(a::InlineString, b::InlineString)
    T = promote_type(typeof(a), typeof(b))
    return isless(T(a), T(b))
end
function Base.cmp(a::InlineString, b::InlineString)
    T = promote_type(typeof(a), typeof(b))
    return cmp(T(a), T(b))
end
function Base.cmp(a::InlineString, b::Union{String, SubString{String}})
    la, lb = ncodeunits(a), sizeof(b)
    ref = Ref(a)
    c = GC.@preserve ref b _memcmp(_ptr(ref), pointer(b), min(la, lb))
    return c == 0 ? cmp(la, lb) : c < 0 ? -1 : 1
end
Base.cmp(a::Union{String, SubString{String}}, b::InlineString) = -cmp(b, a)

@static if isdefined(Base, :hash_bytes)

function Base.hash(x::InlineString, h::UInt)
    ref = Ref(x)
    return GC.@preserve ref Base.hash_bytes(_ptr(ref), ncodeunits(x), UInt64(h), Base.HASH_SECRET) % UInt
end

else

function Base.hash(x::T, h::UInt) where {T <: InlineString}
    h += Base.memhash_seed
    return ccall(Base.memhash, UInt, (Ref{T}, Csize_t, UInt32), Ref(x), sizeof(x), h % UInt32) + h
end

end # @static

#-----------------------------------------------------------------------------
# IO
#-----------------------------------------------------------------------------

# Like `String`, writing an inline string writes its codeunits.
function Base.write(io::IO, x::InlineString)
    n = ncodeunits(x)
    ref = Ref(x)
    GC.@preserve ref unsafe_write(io, _ptr(ref), n % UInt)
    return n
end
# Base's `print(::IO, ::AbstractString)` iterates characters
Base.print(io::IO, x::InlineString) = (write(io, x); nothing)
@static if isdefined(Base, :AnnotatedIOBuffer)
    Base.write(io::Base.AnnotatedIOBuffer, x::InlineString) =
        invoke(write, Tuple{Base.AnnotatedIOBuffer, AbstractString}, io, x)
end

@inline function Base.__unsafe_string!(out, x::InlineString, offs::Integer)
    n = ncodeunits(x)
    ref = Ref(x)
    GC.@preserve ref out unsafe_copyto!(pointer(out, offs), _ptr(ref), n)
    return n
end

#-----------------------------------------------------------------------------
# Whole-string predicates
#-----------------------------------------------------------------------------

_oftype(::Type{T}, x::S) where {T, S} = sizeof(T) == sizeof(S) ? Base.bitcast(T, x) : sizeof(T) > sizeof(S) ? Base.zext_int(T, x) : Base.trunc_int(T, x)

# all unused bytes are zero, so every 64-bit word can be checked at once
@generated function _anyhighbit(d::T, ::Val{K}) where {T, K}
    checks = [:((Base.trunc_int(UInt64, Base.lshr_int(d, $(64 * (k - 1)))) & 0x8080808080808080) != 0 && return true) for k in 1:K]
    return Expr(:block, checks..., :(return false))
end
function Base.isascii(x::T) where {T <: InlineString}
    d = _data(x)
    if sizeof(T) <= 8
        return (_oftype(UInt64, d) & 0x8080808080808080) == 0
    else
        return !_anyhighbit(d, Val(sizeof(T) >> 3))
    end
end

Base.length(s::InlineString) = isascii(s) ? ncodeunits(s) : _length_slow(s)
function _length_slow(s::InlineString)
    n = ncodeunits(s)
    return length_continued(_bytes(s), 1, n, n)
end

#-----------------------------------------------------------------------------
# Sub-string operations (always return the same inline type)
#-----------------------------------------------------------------------------

const NativeUTF8String = Union{String, SubString{String}, InlineString}

function Base.chop(s::InlineString; head::Integer = 0, tail::Integer = 1)
    isempty(s) && return s
    n = ncodeunits(s)
    i = min(n + 1, max(nextind(s, firstindex(s), head), 1))  # first char to keep
    j = max(0, min(n, prevind(s, lastindex(s), tail)))       # last char to keep
    return _sub(s, i, nextind(s, j) - 1)
end

Base.getindex(s::InlineString, r::AbstractUnitRange{<:Integer}) = getindex(s, Int(first(r)):Int(last(r)))

Base.@propagate_inbounds function Base.getindex(s::InlineString, r::UnitRange{Int})
    isempty(r) && return typeof(s)()
    i, j = first(r), last(r)
    @boundscheck checkbounds(s, i:j)
    if sizeof(typeof(s)) > 2
        bytes = _bytes(s)
        # An ASCII byte is always a character boundary. This common path also avoids
        # materializing the byte tuple once each in `isvalid(i)`, `isvalid(j)`, and
        # `nextind(j)` for the wide inline types.
        if @inbounds bytes[i] < 0x80 && bytes[j] < 0x80
            return _sub(s, i, j)
        end
    end
    @boundscheck begin
        @inbounds isvalid(s, i) || Base.string_index_err(s, i)
        @inbounds isvalid(s, j) || Base.string_index_err(s, j)
    end
    return _sub(s, i, nextind(s, j) - 1)
end

Base.view(s::InlineString, r::AbstractUnitRange{<:Integer}) = getindex(s, r)

function Base.chopprefix(s::InlineString, prefix::AbstractString)
    (!isempty(prefix) && startswith(s, prefix)) || return s
    # For non-UTF-8 code units, advance in `s` so its storage determines the
    # byte boundary to keep. Preserve the constant-time UTF-8 path.
    i = if prefix isa NativeUTF8String
        ncodeunits(prefix) + 1
    else
        nextind(s, firstindex(s), length(prefix))
    end
    return _sub(s, i, ncodeunits(s))
end
function Base.chopprefix(s::InlineString, prefix::Regex)
    m = match(prefix, String(s), firstindex(s), Base.PCRE.ANCHORED)
    m === nothing && return s
    return _sub(s, ncodeunits(m.match) + 1, ncodeunits(s))
end

function Base.chopsuffix(s::InlineString, suffix::AbstractString)
    (!isempty(suffix) && endswith(s, suffix)) || return s
    # For non-UTF-8 code units, retreat in `s` so its storage determines the
    # byte boundary to keep. Preserve the constant-time UTF-8 path.
    j = if suffix isa NativeUTF8String
        ncodeunits(s) - ncodeunits(suffix)
    else
        prevind(s, ncodeunits(s) + 1, length(suffix)) - 1
    end
    return _sub(s, 1, j)
end
function Base.chopsuffix(s::InlineString, suffix::Regex)
    m = match(suffix, String(s), firstindex(s), Base.PCRE.ENDANCHORED)
    m === nothing && return s
    return _sub(s, 1, ncodeunits(s) - ncodeunits(m.match))
end

throw_strip_argument_error() =
    throw(ArgumentError("Both arguments are strings. The second argument should be a `Char` or collection of `Char`s"))

function Base.lstrip(f, s::InlineString)
    nc = 0
    for c in s
        f(c) || break
        nc += ncodeunits(c)
    end
    return nc == 0 ? s : _sub(s, nc + 1, ncodeunits(s))
end
Base.lstrip(::AbstractString, ::InlineString) = throw_strip_argument_error()

function Base.rstrip(f, s::InlineString)
    nc = 0
    for c in Iterators.reverse(s)
        f(c) || break
        nc += ncodeunits(c)
    end
    return nc == 0 ? s : _sub(s, 1, ncodeunits(s) - nc)
end
Base.rstrip(::AbstractString, ::InlineString) = throw_strip_argument_error()

function Base.chomp(s::InlineString)
    n = ncodeunits(s)
    if n < 1 || @inbounds(codeunit(s, n)) != 0x0a
        return s
    elseif n < 2 || @inbounds(codeunit(s, n - 1)) != 0x0d
        return _sub(s, 1, n - 1)
    else
        return _sub(s, 1, n - 2)
    end
end

function Base.first(s::InlineString, n::Integer)
    j = min(lastindex(s), nextind(s, 0, n))
    return _sub(s, 1, nextind(s, j) - 1)
end

function Base.last(s::InlineString, n::Integer)
    nc = ncodeunits(s)
    return _sub(s, max(1, prevind(s, nc + 1, n)), nc)
end

Base.reverse(x::String1) = x
function Base.reverse(s::T) where {T <: InlineString}
    n = ncodeunits(s)
    n <= 1 && return s
    if isascii(s)
        # byte-swap the data (leaving it at the top), then shift it back down
        return _withlen(Base.lshr_int(Base.bswap_int(_data(s)), 8 * (sizeof(T) - n)), n)
    end
    bytes = _bytes(s)
    ref = Ref{T}(_zero(T))
    GC.@preserve ref begin
        ptr = Ptr{UInt8}(pointer_from_objref(ref))
        i = 1
        while i <= n
            j = nextind(s, i)     # codeunits i:j-1 form one character
            dest = n - j + 2
            for k = i:(j - 1)
                unsafe_store!(ptr, @inbounds(bytes[k]), dest + (k - i))
            end
            i = j
        end
    end
    return _withlen(ref[], n)
end

# `map` keeps the inline type when the result fits, and falls back to a `String`
# (like the `AbstractString` fallback) when it does not.
@inline function _mapchar(c′)
    isa(c′, AbstractChar) || throw(ArgumentError(
        "map(f, s::AbstractString) requires f to return AbstractChar; " *
        "try map(f, collect(s)) or a comprehension instead"))
    return convert(Char, c′)
end

@noinline function _map_to_string(f, s::InlineString, i::Int, ptr::Ptr{UInt8}, n::Int, c::Char)
    io = IOBuffer(; sizehint=max(ncodeunits(s), n + ncodeunits(c)))
    Base.unsafe_write(io, ptr, UInt(n))
    write(io, c)
    last = ncodeunits(s)
    while i <= last
        value, i = @inbounds iterate(s, i)
        write(io, _mapchar(f(value)))
    end
    return String(take!(io))
end

function Base.map(f, s::T) where {T <: InlineString}
    ref = Ref{T}(_zero(T))
    n = 0
    GC.@preserve ref begin
        ptr = Ptr{UInt8}(pointer_from_objref(ref))
        i = 1
        for c in s
            nexti = i + ncodeunits(c)
            c′ = _mapchar(f(c))
            u = bswap(reinterpret(UInt32, c′))
            k = ncodeunits(c′)
            n + k < sizeof(T) || return _map_to_string(f, s, nexti, ptr, n, c′)
            for m = 1:k
                unsafe_store!(ptr, (u >> (8 * (m - 1))) % UInt8, n + m)
            end
            n += k
            i = nexti
        end
    end
    return _withlen(ref[], n)
end

function Base.filter(f, s::T) where {T <: InlineString}
    n = ncodeunits(s)
    bytes = _bytes(s)
    ref = Ref{T}(_zero(T))
    m = 0
    GC.@preserve ref begin
        ptr = Ptr{UInt8}(pointer_from_objref(ref))
        i = 1
        while i <= n
            c, j = @inbounds iterate(s, i)
            if f(c)
                for k = i:(j - 1)
                    unsafe_store!(ptr, @inbounds(bytes[k]), m + 1 + (k - i))
                end
                m += j - i
            end
            i = j
        end
    end
    return _withlen(ref[], m)
end

#-----------------------------------------------------------------------------
# Concatenation
#-----------------------------------------------------------------------------

Base.string(a::InlineString) = a
Base.string(a::InlineString...) = _string(a...)

@inline function _string(a::InlineString...)
    n = 0
    for v in a
        n += sizeof(v)
    end
    out = Base._string_n(n)
    offs = 1
    for v in a
        offs += Base.__unsafe_string!(out, v, offs)
    end
    return out
end

# For more and/or bigger InlineStrings creating a `Base.String` is faster
const _SmallerInlineStrings = Union{InlineString1, InlineString3, InlineString7}
Base.string(a::_SmallerInlineStrings, b::_SmallerInlineStrings, c::_SmallerInlineStrings) =
    _string(_string(a, b), c)
const _SmallestInlineStrings = Union{InlineString1, InlineString3}
Base.string(a::_SmallestInlineStrings, b::_SmallestInlineStrings, c::_SmallestInlineStrings, d::_SmallestInlineStrings) =
    _string(_string(_string(a, b), c), d)

# Only benefit from keeping the small-ish strings as InlineStrings
function _string(a::Ta, b::Tb) where {Ta <: SmallInlineStrings, Tb <: SmallInlineStrings}
    T = summed_type(Ta, Tb)
    la, lb = ncodeunits(a), ncodeunits(b)
    x = Base.or_int(Base.zext_int(T, _data(a)), Base.shl_int(Base.zext_int(T, _data(b)), 8 * la))
    return _withlen(x, la + lb)
end

summed_type(::Type{InlineString1}, ::Type{InlineString1}) = InlineString3
summed_type(::Type{InlineString3}, ::Type{InlineString1}) = InlineString7
summed_type(::Type{InlineString3}, ::Type{InlineString3}) = InlineString7
summed_type(::Type{InlineString7}, ::Type{InlineString1}) = InlineString15
summed_type(::Type{InlineString7}, ::Type{InlineString3}) = InlineString15
summed_type(::Type{InlineString7}, ::Type{InlineString7}) = InlineString15
summed_type(::Type{InlineString15}, ::Type{InlineString1}) = InlineString31
summed_type(::Type{InlineString15}, ::Type{InlineString3}) = InlineString31
summed_type(::Type{InlineString15}, ::Type{InlineString7}) = InlineString31
summed_type(::Type{InlineString15}, ::Type{InlineString15}) = InlineString31
summed_type(a::Type{<:SmallInlineStrings}, b::Type{<:SmallInlineStrings}) = summed_type(b, a)

function Base.repeat(x::InlineString, r::Integer)
    r < 0 && throw(ArgumentError("can't repeat a string $r times"))
    r == 0 && return ""
    r == 1 && return x
    n = sizeof(x)
    n == 0 && return ""
    out = Base._string_n(n * r)
    if n == 1 # common case: repeating a single-byte string
        @inbounds b = codeunit(x, 1)
        ccall(:memset, Ptr{Cvoid}, (Ptr{UInt8}, Cint, Csize_t), out, b, r)
    else
        ref = Ref(x)
        GC.@preserve ref out for i = 0:r-1
            unsafe_copyto!(pointer(out, i * n + 1), _ptr(ref), n)
        end
    end
    return out
end

#-----------------------------------------------------------------------------
# Searching
#-----------------------------------------------------------------------------

function Base.startswith(a::InlineString, b::InlineString)
    na, nb = ncodeunits(a), ncodeunits(b)
    nb <= na || return false
    T = promote_type(typeof(a), typeof(b))
    return _keeplow(T(a), nb) === _data(T(b)) && nextind(a, nb) == nb + 1
end
function Base.startswith(a::InlineString, b::Union{String, SubString{String}})
    nb = ncodeunits(b)
    ncodeunits(a) < nb && return false
    ref = Ref(a)
    c = GC.@preserve ref b _memcmp(_ptr(ref), pointer(b), nb)
    return c == 0 && nextind(a, nb) == nb + 1
end

function Base.endswith(a::InlineString, b::InlineString)
    na, nb = ncodeunits(a), ncodeunits(b)
    nb <= na || return false
    T = promote_type(typeof(a), typeof(b))
    astart = na - nb + 1
    return Base.lshr_int(_data(T(a)), 8 * (astart - 1)) === _data(T(b)) && thisind(a, astart) == astart
end
function Base.endswith(a::InlineString, b::Union{String, SubString{String}})
    nb = ncodeunits(b)
    astart = ncodeunits(a) - nb + 1
    astart < 1 && return false
    ref = Ref(a)
    c = GC.@preserve ref b _memcmp(_ptr(ref) + (astart - 1), pointer(b), nb)
    return c == 0 && thisind(a, astart) == astart
end

# Byte searches over the (hoisted) byte tuple, matching `String`'s byte-level
# semantics; these back `occursin`, `findfirst`/`findnext`, `findall`, `replace`...
@inline function _searchindex_memcmp(sptr::Ptr{UInt8}, tptr::Ptr{UInt8}, i::Int, last::Int, n::Int)
    b1 = unsafe_load(tptr)
    @inbounds for k = i:last
        unsafe_load(sptr, k) == b1 || continue
        _memcmp(sptr + k, tptr + 1, n - 1) == 0 && return k
    end
    return 0
end

function _searchindex_memcmp(s::InlineString, t::InlineString, i::Int, last::Int, n::Int)
    sref, tref = Ref(s), Ref(t)
    return GC.@preserve sref tref _searchindex_memcmp(_ptr(sref), _ptr(tref), i, last, n)
end

function _searchindex_memcmp(s::InlineString, t::Union{String, SubString{String}}, i::Int, last::Int, n::Int)
    ref = Ref(s)
    return GC.@preserve ref t _searchindex_memcmp(_ptr(ref), pointer(t), i, last, n)
end

function Base._searchindex(s::InlineString, t::Union{String, SubString{String}, InlineString}, i::Integer)
    n, m = ncodeunits(t), ncodeunits(s)
    i = Int(i)
    1 <= i <= m + 1 || throw(BoundsError(s, i))
    n == 0 && return i
    # like `String`, a single-character needle goes through `findnext`, which validates `i`
    lastindex(t) == 1 && return something(findnext(isequal(t[1]), s, i), 0)
    w = m - n
    (w < 0 || i - 1 > w) && return 0
    # libc's vectorized comparison repays its call overhead for long needles.
    n >= 16 && return _searchindex_memcmp(s, t, i, w + 1, n)
    bytes = _bytes(s)
    @inbounds b1 = codeunit(t, 1)
    @inbounds for k = i:(w + 1)
        bytes[k] == b1 || continue
        matched = true
        for l = 2:n
            if bytes[k + l - 1] != codeunit(t, l)
                matched = false
                break
            end
        end
        matched && return k
    end
    return 0
end

function _searchbyte(s::InlineString, b::UInt8, i::Int)
    bytes = _bytes(s)
    @inbounds for k = i:ncodeunits(s)
        bytes[k] == b && return k
    end
    return nothing
end

function Base.findnext(pred::Base.Fix2{<:Union{typeof(isequal), typeof(==)}, <:AbstractChar}, s::InlineString, i::Integer)
    i = Int(i)
    if i < 1 || i > ncodeunits(s)
        i == ncodeunits(s) + 1 && return nothing
        throw(BoundsError(s, i))
    end
    @inbounds isvalid(s, i) || Base.string_index_err(s, i)
    c = pred.x
    c ≤ '\x7f' && return _searchbyte(s, c % UInt8, i)
    b1 = (reinterpret(UInt32, Char(c)) >> 24) % UInt8  # first UTF-8 byte of `c`
    while true
        i = _searchbyte(s, b1, i)
        i === nothing && return nothing
        isvalid(s, i) && pred(s[i]) && return i
        i = nextind(s, i)
    end
end

Base.match(r::Regex, s::InlineString, i::Integer) = match(r, String(s), i)
Base.findnext(r::Regex, s::InlineString, i::Integer) = findnext(r, String(s), i)

#-----------------------------------------------------------------------------
# UTF-8 indexing (ported from Base strings/string.jl)
#-----------------------------------------------------------------------------

Base.isvalid(s::InlineString, i::Int) = checkbounds(Bool, s, i) && (@inbounds thisind(s, i) == i)

Base.@propagate_inbounds function Base.thisind(s::InlineString, i::Int)
    i == 0 && return 0
    n = ncodeunits(s)
    i == n + 1 && return i
    @boundscheck Base.between(i, 1, n) || throw(BoundsError(s, i))
    b = _byte(s, i)
    (b & 0xc0 == 0x80) & (i-1 > 0) || return i
    b = _byte(s, i-1)
    Base.between(b, 0b11000000, 0b11110111) && return i-1
    (b & 0xc0 == 0x80) & (i-2 > 0) || return i
    b = _byte(s, i-2)
    Base.between(b, 0b11100000, 0b11110111) && return i-2
    (b & 0xc0 == 0x80) & (i-3 > 0) || return i
    b = _byte(s, i-3)
    Base.between(b, 0b11110000, 0b11110111) && return i-3
    return i
end

Base.@propagate_inbounds function Base.nextind(s::InlineString, i::Int)
    i == 0 && return 1
    n = ncodeunits(s)
    @boundscheck Base.between(i, 1, n) || throw(BoundsError(s, i))
    l = _byte(s, i)
    (l < 0x80) | (0xf8 ≤ l) && return i+1
    if l < 0xc0
        i′ = @inbounds thisind(s, i)
        return i′ < i ? @inbounds(nextind(s, i′)) : i+1
    end
    # first continuation byte
    (i += 1) > n && return i
    b = _byte(s, i)
    b & 0xc0 ≠ 0x80 && return i
    ((i += 1) > n) | (l < 0xe0) && return i
    # second continuation byte
    b = _byte(s, i)
    b & 0xc0 ≠ 0x80 && return i
    ((i += 1) > n) | (l < 0xf0) && return i
    # third continuation byte
    b = _byte(s, i)
    ifelse(b & 0xc0 ≠ 0x80, i, i+1)
end

Base.@propagate_inbounds function Base.iterate(s::InlineString, i::Int=firstindex(s))
    (i % UInt) - 1 < ncodeunits(s) || return nothing
    b = _byte(s, i)
    u = UInt32(b) << 24
    Base.between(b, 0x80, 0xf7) || return reinterpret(Char, u), i+1
    return iterate_continued(s, i, u)
end

function iterate_continued(s::InlineString, i::Int, u::UInt32)
    u < 0xc0000000 && (i += 1; @goto ret)
    n = ncodeunits(s)
    bytes = _bytes(s)
    # first continuation byte
    (i += 1) > n && @goto ret
    @inbounds b = bytes[i]
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 16
    # second continuation byte
    ((i += 1) > n) | (u < 0xe0000000) && @goto ret
    @inbounds b = bytes[i]
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 8
    # third continuation byte
    ((i += 1) > n) | (u < 0xf0000000) && @goto ret
    @inbounds b = bytes[i]
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b); i += 1
@label ret
    return reinterpret(Char, u), i
end

Base.@propagate_inbounds function Base.getindex(s::InlineString, i::Integer)
    b = codeunit(s, i)
    u = UInt32(b) << 24
    Base.between(b, 0x80, 0xf7) || return reinterpret(Char, u)
    return getindex_continued(s, Int(i), u)
end

function getindex_continued(s::InlineString, i::Int, u::UInt32)
    if u < 0xc0000000
        # called from `getindex` which checks bounds
        @inbounds isvalid(s, i) && @goto ret
        Base.string_index_err(s, i)
    end
    n = ncodeunits(s)
    bytes = _bytes(s)

    (i += 1) > n && @goto ret
    @inbounds b = bytes[i] # cont byte 1
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 16

    ((i += 1) > n) | (u < 0xe0000000) && @goto ret
    @inbounds b = bytes[i] # cont byte 2
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b) << 8

    ((i += 1) > n) | (u < 0xf0000000) && @goto ret
    @inbounds b = bytes[i] # cont byte 3
    b & 0xc0 == 0x80 || @goto ret
    u |= UInt32(b)
@label ret
    return reinterpret(Char, u)
end

Base.@propagate_inbounds function Base.length(s::InlineString, i::Int, j::Int)
    @boundscheck begin
        0 < i ≤ ncodeunits(s)+1 || throw(BoundsError(s, i))
        0 ≤ j < ncodeunits(s)+1 || throw(BoundsError(s, j))
    end
    j < i && return 0
    @inbounds i, k = thisind(s, i), i
    c = j - i + (i == k)
    length_continued(_bytes(s), i, j, c)
end

# `bytes` is the byte tuple of the string; count characters in codeunits i:n
@inline function length_continued(bytes::NTuple{N, UInt8}, i::Int, n::Int, c::Int) where {N}
    i < n || return c
    @inbounds b = bytes[i]
    @inbounds while true
        while true
            (i += 1) ≤ n || return c
            0xc0 ≤ b ≤ 0xf7 && break
            b = bytes[i]
        end
        l = b
        b = bytes[i] # cont byte 1
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xe0) || continue

        (i += 1) ≤ n || return c
        b = bytes[i] # cont byte 2
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xf0) || continue

        (i += 1) ≤ n || return c
        b = bytes[i] # cont byte 3
        c -= (b & 0xc0 == 0x80)
    end
end

#-----------------------------------------------------------------------------
# Sorting: teach Base's radix sort how to map the small inline strings to
# unsigned integers, so `sort`, `sort!`, `sortperm`, `rev=true` and arrays with
# `missing` all get Base's optimized pipeline for free.
#-----------------------------------------------------------------------------

import Base.Sort: UIntMappable, uint_map, uint_unmap
using Base.Order: ForwardOrdering

for (T, U) in ((:String1, :UInt16), (:String3, :UInt32), (:String7, :UInt64), (:String15, :UInt128))
    @eval begin
        UIntMappable(::Type{$T}, ::ForwardOrdering) = $U
        uint_map(x::$T, ::ForwardOrdering) = Base.bitcast($U, _key(x))
        uint_unmap(::Type{$T}, u::$U, ::ForwardOrdering) = _unkey(Base.bitcast($T, u))
    end
end

#-----------------------------------------------------------------------------
# Collections of InlineStrings
#-----------------------------------------------------------------------------

"""
    inlinestrings(itr) => Vector

    Utility function that takes any iterator of `AbstractString` values
and attempts to produce a `Vector` with a single promoted `InlineString` type. That is,
all iterated elements will be promoted to the smallest `InlineString` subtype
that can fit all elements. If any value is larger than the current largest InlineString
type (256 bytes), the entire collection will be promoted to `String` instead.
`missing` values are also allowed and will result in a result eltype of `Union{Missing, X}`
where `X` is an `InlineString` subtype or `String`.
"""
function inlinestrings(itr::T) where {T}
    # x must be iterable
    IS = Base.IteratorSize(T)
    state = iterate(itr)
    state === nothing && return []
    y, st = state
    y = y === missing ? missing : _utf8_source(y)
    x = y === missing ? missing : ncodeunits(y) < 256 ? InlineString(y) : String(y)
    eT = typeof(x)
    # allocate res, which will either be same length as `itr` if
    # IS <: HasLength, or length of 0 if Base.SizeUnknown
    res = allocate(eT, IS, itr)
    i = 1
    # set! push!-es for Base.SizeUnknown, or setindex! for HasLength
    set!(IS, res, x, i)
    i += 1
    # dispatch to separate function for type stability
    return _inlinestrings(itr, st, eT, IS, res, i)
end

const HasLength = Union{Base.HasShape, Base.HasLength}
allocate(::Type{T}, ::HasLength, itr) where {T} = Vector{T}(undef, length(itr))
allocate(::Type{T}, IS, itr) where {T} = Vector{T}(undef, 0)
set!(::HasLength, res, x, i) = setindex!(res, x, i)
set!(IS, res, x, i) = push!(res, x)
_utf8_source(y::AbstractString) = codeunit(y) === UInt8 ? y : String(y)
_fits_inline_storage(::Type{String}, y) = true
_fits_inline_storage(::Type{T}, y) where {T} = ncodeunits(y) < sizeof(T)

function _inlinestrings(itr, st, ::Type{eT}, IS, res, i) where {eT}
    while true
        state = iterate(itr, st)
        state === nothing && break
        y, st = state
        y = y === missing ? missing : _utf8_source(y)
        if y === missing && eT >: Missing
            set!(IS, res, missing, i)
        elseif y !== missing && eT !== Missing && _fits_inline_storage(Base.nonmissingtype(eT), y)
            set!(IS, res, Base.nonmissingtype(eT)(y), i)
        else
            # need to promote and widen res,
            # then re-dispatch on _inlinestrings for new eltype
            x = y === missing ? missing : ncodeunits(y) < 256 ? InlineString(y) : String(y)
            new_eT = promote_type(typeof(x), eT)
            newres = allocate(new_eT, Base.HasLength(), res)
            copyto!(newres, 1, res, 1, i - 1)
            set!(IS, newres, x, i)
            return _inlinestrings(itr, st, new_eT, IS, newres, i + 1)
        end
        i += 1
    end
    return res
end

_inlinestrings_array(A::AbstractArray) = reshape(inlinestrings(A), size(A))

Base.Broadcast.broadcasted(::Type{InlineString}, A::AbstractArray) = _inlinestrings_array(A)
# restricted to `Array` to avoid ambiguities with the `map` methods of LinearAlgebra's
# structured matrices; `InlineString.(A)` works for any array
Base.map(::Type{InlineString}, A::Array) = _inlinestrings_array(A)
Base.collect(::Type{InlineString}, A::Array) = _inlinestrings_array(A)

end # module
