using Test, InlineStrings, Parsers, Serialization, Random, Aqua

const _PARSERS_HAS_XPARSE = isdefined(Parsers, :xparse)
if _PARSERS_HAS_XPARSE
    import Parsers: SENTINEL, OK, EOF, OVERFLOW, QUOTED, DELIMITED,
                    INVALID_DELIMITER, INVALID_QUOTED_FIELD, ESCAPED_STRING,
                    NEWLINE, SUCCESS
end

@testset "Parsers extension loading" begin
    if isdefined(Base, :get_extension)
        @test Base.get_extension(InlineStrings, :ParsersExt) isa Module
    else
        @test isdefined(InlineStrings, :ParsersExt)
    end
end

struct UTF16TestString <: AbstractString
    data::Vector{UInt16}
end
Base.codeunit(::UTF16TestString) = UInt16
Base.ncodeunits(s::UTF16TestString) = length(s.data)
Base.codeunit(s::UTF16TestString, i::Integer) = s.data[i]
Base.String(s::UTF16TestString) = transcode(String, s.data)

const SUBTYPES = (
    InlineString1,
    InlineString3,
    InlineString7,
    InlineString15,
    InlineString31,
    InlineString63,
    InlineString127,
    InlineString255,
)

@testset "InlineString basics" begin

y = "abcdef"
x = InlineString(y)
x, overflow = InlineStrings.addcodeunit(x, UInt8('g'))
@test !overflow
@test x == "abcdefg"
x, overflow = InlineStrings.addcodeunit(x, UInt8('g'))
@test overflow

x = InlineString("abc")
@test x == InlineString7(x) == InlineString15(x) == InlineString31(x) == InlineString63(x)
@test x == InlineString127(x) == InlineString255(x)
y = InlineString7(x)
@test InlineString3(y) == x
@test_throws ArgumentError InlineString3(InlineString("abcd"))
@test_throws ArgumentError InlineString1(InlineString("ab"))
x = InlineString("a")
y = InlineString7(x)
@test x == y
@test InlineString1(y) == x
@test InlineString1(x) == x

@test promote_type(InlineString1, InlineString3) === InlineString3
@test promote_type(InlineString1, InlineString255) === InlineString255
@test promote_type(InlineString31, InlineString127) === InlineString127
@test promote_type(InlineString255, InlineString7) === InlineString255
@test promote_type(InlineString63, InlineString15) === InlineString63

# Ensure we haven't caused ambiguity with Base.
# https://discourse.julialang.org/t/having-trouble-implementating-a-tables-jl-row-table-when-using-badukgoweiqitools-dataframe-tbl-no-longer-works/63622/1
@test promote_type(Union{}, String) == String

# construction from bytes buffer
# make sure we don't read past an end of a buffer
buf = Vector{UInt8}("hey")
x = InlineString7(buf, 1, 3)
@test x == "hey"
@test typeof(x) == InlineString7
x = InlineString7(buf)
@test x == "hey"
@test typeof(x) == InlineString7
@test_throws ArgumentError InlineString7(b"abcdefgh")
@test_throws ArgumentError InlineString7(buf, 3, 2)
@test_throws ArgumentError InlineString7(buf, 0, 1)
@test_throws ArgumentError InlineString7(buf, 1, -1)
@test InlineString7(buf, 2) === InlineString7("ey")
@test InlineString7(buf, 4) === InlineString7("")
@test InlineString7(view(buf, 2:3)) === InlineString7("ey")
@test InlineString7(0x61:0x63) === InlineString7("abc") # generic AbstractVector path
@test InlineString7(0x61:0x63, 2, 1) === InlineString7("b")
@test InlineString7(codeunits("hey")) === InlineString7("hey")
@test InlineString7(codeunits(InlineString3("hey"))) === InlineString7("hey")

buf = Vector{UInt8}("x")
x = InlineString1(buf, 1, 1)
@test x == "x"
@test typeof(x) == InlineString1
x = InlineString1(buf)
@test x == "x"
@test typeof(x) == InlineString1
@test_throws ArgumentError InlineString1(b"xy")

# https://github.com/JuliaData/WeakRefStrings.jl/issues/88
@test InlineString(String1("a")) === String1("a")

utf16 = UTF16TestString(transcode(UInt16, "éβ"))
@test InlineString(utf16) === String7("éβ")
@test String7(utf16) === String7("éβ")
@test_throws ArgumentError String3(utf16)

# SubString sources
@test String7(SubString("xhelloy", 2, 6)) === String7("hello")
@test String15(SubString(String7("hello"), 2, 4)) === String15("ell")
@test String3(SubString(String255("hello"), 2, 4)) === String3("ell")
@test_throws ArgumentError String3(SubString(String255("hello"), 1, 4))

# https://github.com/JuliaData/InlineStrings.jl/issues/2
@test eltype(string.(AbstractString[])) == AbstractString

# https://github.com/JuliaData/InlineStrings.jl/issues/8
# construction from pointer
ptrstr = "hey"
@test_throws ArgumentError String1(Ptr{UInt8}(0))
@test_throws ArgumentError String1(pointer(ptrstr), 2)
@test String1(pointer(ptrstr), 1) === String1("h")
ptrstr1 = UInt8['h', 0x00]
@test String1(pointer(ptrstr1)) === String1("h")
ptrstr2 = UInt8['h', 'e', 0x00]
@test_throws ArgumentError String1(pointer(ptrstr2))
@test String3(pointer(ptrstr1)) === String3("h")
ptrstr3 = UInt8['h', 'e', 'y', '1', 0x00]
@test_throws ArgumentError String3(pointer(ptrstr3))
@test_throws ArgumentError String3(pointer(ptrstr3), 4)
@test String3(pointer(ptrstr3), 3) === String3("hey")
@test_throws ArgumentError String3(pointer(ptrstr3), -1)
@test_throws ArgumentError InlineStringType(-1)

# https://github.com/JuliaStrings/InlineStrings.jl/issues/32
abc = InlineString3("abc")
@test first(abc, 2) == InlineString3("ab")
@test last(abc, 2) == InlineString3("bc")
@test chop(abc) == InlineString3("ab")
@test chop(abc; head=1, tail=0) == InlineString3("bc")
@test chop(abc) isa InlineString3
@test chomp(InlineString3("ab\n")) == InlineString3("ab")
@test chomp(InlineString7("ab\r\n")) == InlineString7("ab")
@test chomp(InlineString7("ab\r\n")) isa InlineString7

# https://github.com/JuliaStrings/InlineStrings.jl/issues/38
@test chop(InlineString7("Bün")) == InlineString7("Bü")

# chop tests copied from Base
# https://github.com/JuliaLang/julia/blob/v1.8.2/test/strings/util.jl#L497-L517
S = InlineString15
@test chop(S("")) == ""
@test chop(S("fooε")) == "foo"
@test chop(S("foεo")) == "foε"
@test chop(S("∃∃∃∃")) == "∃∃∃"
@test chop(S("∀ϵ∃Δ"), head=0, tail=0) == "∀ϵ∃Δ"
@test chop(S("∀ϵ∃Δ"), head=0, tail=1) == "∀ϵ∃"
@test chop(S("∀ϵ∃Δ"), head=0, tail=2) == "∀ϵ"
@test chop(S("∀ϵ∃Δ"), head=0, tail=3) == "∀"
@test chop(S("∀ϵ∃Δ"), head=0, tail=4) == ""
@test chop(S("∀ϵ∃Δ"), head=0, tail=5) == ""
@test chop(S("∀ϵ∃Δ"), head=1, tail=0) == "ϵ∃Δ"
@test chop(S("∀ϵ∃Δ"), head=2, tail=0) == "∃Δ"
@test chop(S("∀ϵ∃Δ"), head=3, tail=0) == "Δ"
@test chop(S("∀ϵ∃Δ"), head=4, tail=0) == ""
@test chop(S("∀ϵ∃Δ"), head=5, tail=0) == ""
@test chop(S("∀ϵ∃Δ"), head=1, tail=1) == "ϵ∃"
@test chop(S("∀ϵ∃Δ"), head=2, tail=2) == ""
@test chop(S("∀ϵ∃Δ"), head=3, tail=3) == ""
@test_throws ArgumentError chop(S("∀ϵ∃Δ"), head=-3, tail=3)
@test_throws ArgumentError chop(S("∀ϵ∃Δ"), head=3, tail=-3)
@test_throws ArgumentError chop(S("∀ϵ∃Δ"), head=-3, tail=-3)

# check that writing code units works as expected
io = IOBuffer()
cu = codeunits(InlineString("hello"))
@test write(io, cu) == 5
@test take!(io) == codeunits("hello")

if isdefined(Base, :chopprefix)
@test chopprefix(abc, "a") === InlineString3("bc")
@test chopprefix(abc, "bc") === abc
@test chopprefix(abc, "abc") === InlineString3("")
@test chopprefix(InlineString1("a"), "a") === InlineString1("")
# Regex case
@test chopprefix(InlineString15("∃∃∃b∃"), r"∃+") === InlineString15("b∃")
@test chopprefix(InlineString1("a"), r".") === InlineString1("")
end

if isdefined(Base, :chopsuffix)
@test chopsuffix(abc, "a") === abc
@test chopsuffix(abc, "bc") === InlineString3("a")
@test chopsuffix(abc, "abc") === InlineString3("")
@test chopsuffix(InlineString1("c"), "c") === InlineString1("")
# Regex case
@test chopsuffix(InlineString15("∃b∃∃∃"), r"∃+") === InlineString15("∃b")
@test chopsuffix(InlineString1("c"), r".") === InlineString1("")
end

if isdefined(Base, :chopprefix) && isdefined(Base, :chopsuffix)
# chopprefix / chopsuffix tests copied from Base
# https://github.com/JuliaLang/julia/blob/v1.8.2/test/strings/util.jl#L519-L564
S = InlineString15
for T in (String, InlineString)
    @test chopprefix(S("fo∀\n"), T("bog")) == "fo∀\n"
    @test chopprefix(S("fo∀\n"), T("\n∀foΔ")) == "fo∀\n"
    @test chopprefix(S("fo∀\n"), T("∀foΔ")) == "fo∀\n"
    @test chopprefix(S("fo∀\n"), T("f")) == "o∀\n"
    @test chopprefix(S("fo∀\n"), T("fo")) == "∀\n"
    @test chopprefix(S("fo∀\n"), T("fo∀")) == "\n"
    @test chopprefix(S("fo∀\n"), T("fo∀\n")) == ""
    @test chopprefix(S("\nfo∀"), T("bog")) == "\nfo∀"
    @test chopprefix(S("\nfo∀"), T("\n∀foΔ")) == "\nfo∀"
    @test chopprefix(S("\nfo∀"), T("\nfo∀")) == ""
    @test chopprefix(S("\nfo∀"), T("\n")) == "fo∀"
    @test chopprefix(S("\nfo∀"), T("\nf")) == "o∀"
    @test chopprefix(S("\nfo∀"), T("\nfo")) == "∀"
    @test chopprefix(S("\nfo∀"), T("\nfo∀")) == ""
    @test chopprefix(S(""), T("")) == ""
    @test chopprefix(S(""), T("asdf")) == ""
    @test chopprefix(S(""), T("∃∃∃")) == ""
    @test chopprefix(S("εfoo"), T("ε")) == "foo"
    @test chopprefix(S("ofoε"), T("o")) == "foε"
    @test chopprefix(S("∃∃∃∃"), T("∃")) == "∃∃∃"
    @test chopprefix(S("∃∃∃∃"), T("")) == "∃∃∃∃"

    @test chopsuffix(S("fo∀\n"), T("bog")) == "fo∀\n"
    @test chopsuffix(S("fo∀\n"), T("\n∀foΔ")) == "fo∀\n"
    @test chopsuffix(S("fo∀\n"), T("∀foΔ")) == "fo∀\n"
    @test chopsuffix(S("fo∀\n"), T("\n")) == "fo∀"
    @test chopsuffix(S("fo∀\n"), T("∀\n")) == "fo"
    @test chopsuffix(S("fo∀\n"), T("o∀\n")) == "f"
    @test chopsuffix(S("fo∀\n"), T("fo∀\n")) == ""
    @test chopsuffix(S("\nfo∀"), T("bog")) == "\nfo∀"
    @test chopsuffix(S("\nfo∀"), T("\n∀foΔ")) == "\nfo∀"
    @test chopsuffix(S("\nfo∀"), T("\nfo∀")) == ""
    @test chopsuffix(S("\nfo∀"), T("∀")) == "\nfo"
    @test chopsuffix(S("\nfo∀"), T("o∀")) == "\nf"
    @test chopsuffix(S("\nfo∀"), T("fo∀")) == "\n"
    @test chopsuffix(S("\nfo∀"), T("\nfo∀")) == ""
    @test chopsuffix(S(""), T("")) == ""
    @test chopsuffix(S(""), T("asdf")) == ""
    @test chopsuffix(S(""), T("∃∃∃")) == ""
    @test chopsuffix(S("fooε"), T("ε")) == "foo"
    @test chopsuffix(S("εofo"), T("o")) == "εof"
    @test chopsuffix(S("∃∃∃∃"), T("∃")) == "∃∃∃"
    @test chopsuffix(S("∃∃∃∃"), T("")) == "∃∃∃∃"
end
end # isdefined


S = InlineString7
@test rstrip(S(" a b c ")) isa S
@test rstrip(S(" a b c ")) === S(" a b c")
@test rstrip(isnumeric, S("abc0123")) === S("abc")
@test rstrip(S("ello"), ['e','o']) === S("ell")
@test rstrip(InlineString1("x")) === InlineString1("x")
@test_throws ArgumentError rstrip("test", S(" a b c "))
@test_throws ArgumentError rstrip("test", InlineString1("x"))

@test lstrip(S(" a b c ")) isa S
@test lstrip(S(" a b c ")) === S("a b c ")
@test lstrip(isnumeric, S("0123abc")) === S("abc")
@test lstrip(S("ello"), ['e','o']) === S("llo")
@test lstrip(InlineString1("x")) === InlineString1("x")
@test_throws ArgumentError lstrip("test", S(" a b c "))
@test_throws ArgumentError lstrip("test", InlineString1("x"))

@test strip(InlineString1("x")) === InlineString1("x")
S = InlineString3
@test strip(S("")) === S("")
@test strip(S(" ")) === S("")
@test strip(S("  ")) === S("")
S = InlineString31
@test strip(S("\t  hi   \n")) === S("hi")
@test strip(S(" \u2009 hi \u2009 ")) === S("hi")
@test strip(S("foobarfoo"), ['f','o']) === S("bar")
@test strip(S("foobarfoo"), ('f','o')) === S("bar")
@test strip(ispunct, S("¡Hola!")) === S("Hola")

# getindex
for S in SUBTYPES
    if S == InlineString1
        x = S("x")
        @test x[1] == 'x'
        @test x[1:1] isa InlineString1
        @test x[1:1] === view(x, 1:1) === InlineString1(x)
        @test x[2:1] === view(x, 2:1) === InlineString1("")
        @test_throws BoundsError x[2]
    else
        abc = S("abc")
        @test abc[1] == 'a'
        @test abc[1:2] isa S
        @test abc[1:2] === view(abc, 1:2) === S("ab")
        @test abc[2:1] === view(abc, 2:1) === S("")
        @test abc[Base.OneTo(2)] === S("ab")
        @test_throws BoundsError abc[4]
        @test_throws BoundsError abc[1:4]
        @test S("÷2")[1:3] === S("÷2")
        @test_throws StringIndexError S("÷2")[2]
        @test_throws StringIndexError S("÷2")[1:2]
    end
end

# repeat one-time should return the same object
@test repeat(InlineString("abc"), 1) === InlineString("abc")

# can't contain NUL when converting to Cstring
@test_throws ArgumentError Base.cconvert(Cstring, InlineString("a\0c"))

end # @testset

const STRINGS = ["", "🍕", "a", "a"^3, "a"^7, "a"^15, "a"^31, "a"^63, "a"^127, "a"^255]
const INLINES = map(InlineString, STRINGS)

@testset "InlineString operations" begin
    for (x, y) in zip(INLINES, STRINGS)
        @test codeunits(x) == codeunits(y)
        @test sizeof(x) == sizeof(y)
        @test ncodeunits(x) == ncodeunits(y)
        @test length(x) == length(y)
        @test codeunit(x) == UInt8
        @test lastindex(x) == lastindex(y)
        @test isempty(x) == isempty(y)
        @test String(x) === y
        @test Symbol(x) == Symbol(y)
        @test Vector{UInt8}(x) == Vector{UInt8}(y)
        @test Array{UInt8}(x) == Array{UInt8}(y)
        @test isascii(x) == isascii(y)
        @test x * x == y * y
        @test x * x * x == y * y * y
        @test x^5 == y^5
        @test string(x) == string(y)
        @test join([x, x]) == join([y, y])
        @test reverse(x) == reverse(y)
        y != "" && @test x[1] == y[1]
        y != "" && @test x[Int8(1)] == y[Int8(1)]
        y != "" && @test x[1:1] == y[1:1]
        y != "" && @test view(x, 1:1) == view(y, 1:1)
        y != "" && @test startswith(x, "a") == startswith(y, "a")
        y != "" && @test endswith(x, "a") == endswith(y, "a")
        y != "" && @test findfirst(==(x[1]), x) === findfirst(==(x[1]), y)
        y != "" && @test findlast(==(x[1]), x) === findlast(==(x[1]), y)
        y != "" && @test findnext(==(x[1]), x, 1) === findnext(==(x[1]), y, 1)
        y != "" && @test findprev(==(x[1]), x, length(x)) === findprev(==(x[1]), y, length(x))
        @test lpad(x, 12) == lpad(y, 12)
        @test rpad(x, 12) == rpad(y, 12)
        y != "" && @test replace(x, x[1] => 'a') == replace(y, x[1] => 'a')
        r1 = match(Regex(x), x)
        r2 = match(Regex(y), y)
        @test r1 === r2 || r1.match == r2.match
        # https://github.com/JuliaStrings/InlineStrings.jl/issues/29
        @test findnext(r"a", x, 1) == findnext(r"a", y, 1)
        @test findall(r"a", x) == findall(r"a", y)
        for i = 1:ncodeunits(x)
            @test codeunit(x, i) == codeunit(y, i)
            @test isvalid(x, i) == isvalid(y, i)
            @test thisind(x, i) == thisind(y, i)
            @test nextind(x, i) == nextind(y, i)
            @test prevind(x, i) == prevind(y, i)
            @test iterate(x, i) == iterate(y, i)
        end
        for i = 1:length(x)
            if isvalid(x, i)
                @test x[i] == y[i]
            end
        end
        @test x == x
        @test x == y
        @test y == x
        @test hash(x) == hash(y)
        @test cmp(x, x) == 0
        @test cmp(x, y) == 0
        @test cmp(y, x) == 0
        # `write` writes the codeunits, like `String`
        io = IOBuffer()
        @test write(io, x) == sizeof(y)
        @test String(take!(io)) == y
        @test write(io, codeunits(x)) == sizeof(y)
        @test String(take!(io)) == y
        @test sprint(print, x) == y
        @test isvalid(x) == isvalid(y)
        @test collect(x) == collect(y)
        @test collect(codeunits(x)) == collect(codeunits(y))
        @test cmp(x, y) == cmp(y, x) == 0
        @test isless(x, y * "a") && !isless(y * "a", x)
        sizeof(y) < 255 && @test isless(x, InlineString255(y * "a")) && !isless(InlineString255(y * "a"), x)
        @test cmp(x, InlineString255(y)) == cmp(InlineString255(y), x) == 0
        @test x == InlineString255(y) && InlineString255(y) == x
        @test x == SubString(y) && SubString(y) == x
        @test chomp(x) == chomp(y)
        if typeof(x) != String255
            @test chomp(InlineString(x * "\n")) == chomp(y * "\n")
            @test chomp(InlineString(x * "\r\n")) == chomp(y * "\r\n")
        end
        @test chop(x) == chop(y)
        @test chop(x; head=1, tail=0) == chop(y; head=1, tail=0)
        @test chop(x; head=1) == chop(y; head=1)
        @test chop(x; head=sizeof(x) + 1) == chop(y; head=sizeof(y) + 1)
        @test chop(x; head=sizeof(x) + 1, tail=sizeof(x) + 1) == chop(y; head=sizeof(x) + 1, tail=sizeof(y) + 1)
        y != "" && @test first(x) == first(y)
        @test first(x, sizeof(x)) == first(y, sizeof(y))
        y != "" && @test first(x, sizeof(x) - 1) == first(y, sizeof(y) - 1)
        @test first(x, sizeof(x) + 1) == first(y, sizeof(y) + 1)
        y != "" && @test last(x) == last(y)
        @test last(x, sizeof(x)) == last(y, sizeof(y))
        y != "" && @test last(x, sizeof(x) - 1) == last(y, sizeof(y) - 1)
        @test last(x, sizeof(x) + 1) == last(y, sizeof(y) + 1)
        # https://github.com/JuliaDatabases/SQLite.jl/issues/306
        ref_uint8 = Base.cconvert(Ptr{UInt8}, x)
        @test GC.@preserve ref_uint8 unsafe_string(Base.unsafe_convert(Ptr{UInt8}, ref_uint8)) == y
        ref_int8 = Base.cconvert(Ptr{Int8}, x)
        @test GC.@preserve ref_int8 unsafe_string(Base.unsafe_convert(Ptr{Int8}, ref_int8)) == y
        ref_cstring = Base.cconvert(Cstring, x)
        @test GC.@preserve ref_cstring unsafe_string(Base.unsafe_convert(Cstring, ref_cstring)) == y
    end

    for y in ("é", "∀", "aé", "éa", "a"^30 * "é", "é" * "a"^30, "🍕" * "a"^250)
        x = InlineString(y)
        @test !isascii(x)
        @test length(x) == length(y)
        @test reverse(x) == reverse(y)
        @test collect(x) == collect(y)
    end
    for invalid in (String(UInt8[0xc0, 0x80]), String(UInt8[0x80, 0x61, 0xe2, 0x82]), String(UInt8[0xf0, 0x9f, 0x8d]))
        x = InlineString(invalid)
        @test codeunits(reverse(x)) == codeunits(reverse(invalid))
        @test length(x) == length(invalid)
        @test collect(x) == collect(invalid)
        @test [isvalid(x, i) for i in 1:ncodeunits(x)] == [isvalid(invalid, i) for i in 1:ncodeunits(invalid)]
    end

    if isdefined(Base, :AnnotatedIOBuffer)
        x = InlineString("abc")
        aio = Base.AnnotatedIOBuffer()
        @test write(aio, x) == 3
        @test String(take!(aio.io)) == "abc"
    end

    a = String(UInt8[0xf6, 0xc8])
    b = String(UInt8[0xf6, 0xb4])
    for x in (a, InlineString(a), InlineString7(a))
        for y in (b, InlineString(b), InlineString7(b))
            @test cmp(x, y) == cmp(a, b) == 1
            @test !isless(x, y)
        end
    end

    x = InlineString("é")
    for i in (-1, 0, ncodeunits(x) + 1, ncodeunits(x) + 2)
        @test isvalid(x, i) == isvalid(String(x), i) == false
    end
end

@testset "`string` / `*`" begin
    # Check `string` overload handles `String1` being concat with other small InlineStrings,
    # because it is easy to mishandle `String1` as it doesn't have a length byte.
    a = "a"
    @test String1(a) * String1(a) == a * a
    @test String1(a) * String1(a) isa InlineString3
    b = "bb"
    @test String1(a) * String3(b) == a * b
    @test String1(a) * String3(b) isa InlineString7
    @test String1(a) * String7(b) == a * b
    @test String1(a) * String7(b) isa InlineString15
    @test String1(a) * String15(b) == a * b
    @test String1(a) * String15(b) isa InlineString31
    @test String1(a) * String3(b) * String7(b) == a * b * b
    @test String1(a) * String3(b) * String7(b) isa InlineString15
    # Check some other combination of small inline strings also work as expected
    @test String3(a) * String7(b) == a * b
    @test String3(a) * String7(b) isa InlineString15
    @test String3(a) * String3(b) * String7(b) == a * b * b
    @test String3(a) * String3(b) * String7(b) isa InlineString15
end

if _PARSERS_HAS_XPARSE
@testset "InlineString parsing with Parsers 2" begin
testcases = [
    ("", InlineString7(""), NamedTuple(), OK | EOF),
    (" ", InlineString7(" "), NamedTuple(), OK | EOF),
    (" \"", InlineString7(), NamedTuple(), OK | QUOTED | EOF | INVALID_QUOTED_FIELD), # invalid quoted
    (" \"\" ", InlineString7(), NamedTuple(), OK | QUOTED | EOF), # quoted
    (" \" ", InlineString7(" "), NamedTuple(), OK | QUOTED | INVALID_QUOTED_FIELD | EOF), # invalid quoted
    (" \" \" ", InlineString7(" "), NamedTuple(), OK | QUOTED | EOF), # quoted
    ("NA", InlineString7(), (; sentinel=["NA"]), EOF | SENTINEL), # sentinel
    ("\"\"", InlineString7(), NamedTuple(), OK | QUOTED | EOF), # same e & cq
    ("\"\",", InlineString7(), NamedTuple(), OK | QUOTED | DELIMITED), # same e & cq
    ("\"\"\"\"", InlineString7("\""), NamedTuple(), OK | QUOTED | ESCAPED_STRING | EOF), # same e & cq
    ("\"\\", InlineString7(), (; escapechar=UInt8('\\')), OK | QUOTED | ESCAPED_STRING | INVALID_QUOTED_FIELD | EOF), # \\ e, invalid quoted
    ("\"\\\"\"", InlineString7("\""), (; escapechar=UInt8('\\')), OK | QUOTED | ESCAPED_STRING | EOF), # \\ e, valid
    ("\"\"", InlineString7(), (; escapechar=UInt8('\\')), OK | QUOTED | EOF), # diff e & cq
    ("\"a", InlineString7("a"), NamedTuple(), OK | QUOTED | INVALID_QUOTED_FIELD | EOF), # invalid quoted
    ("\"a\"", InlineString7("a"), NamedTuple(), OK | QUOTED | EOF), # quoted
    ("\"a\" ", InlineString7("a"), NamedTuple(), OK | QUOTED | EOF), # quoted
    ("\"a\",", InlineString7("a"), NamedTuple(), OK | QUOTED | DELIMITED), # quoted
    ("a,", InlineString7("a"), NamedTuple(), OK | DELIMITED),
    ("a__", InlineString7("a"), (; delim="__"), OK | DELIMITED),
    ("a,", InlineString7("a"), (; ignorerepeated=true), OK | DELIMITED),
    ("a__", InlineString7("a"), (; delim="__", ignorerepeated=true), OK | DELIMITED),
    ("a\n", InlineString7("a"), (; ignorerepeated=true), OK | NEWLINE | EOF),
    ("a\r", InlineString7("a"), (; ignorerepeated=true), OK | NEWLINE | EOF),
    ("a\r\n", InlineString7("a"), (; ignorerepeated=true), OK | NEWLINE | EOF),
    ("a", InlineString7("a"), (; ignorerepeated=true), OK | EOF),
    ("a,,\n", InlineString7("a"), (; ignorerepeated=true), OK | DELIMITED | NEWLINE | EOF),
    ("a\n", InlineString7("a"), (; delim="__", ignorerepeated=true), OK | NEWLINE | EOF),
    ("a\r", InlineString7("a"), (; delim="__", ignorerepeated=true), OK | NEWLINE | EOF),
    ("a\r\n", InlineString7("a"), (; delim="__", ignorerepeated=true), OK | NEWLINE | EOF),
    ("a", InlineString7("a"), (; delim="__", ignorerepeated=true), OK | EOF),
    ("a____\n", InlineString7("a"), (; delim="__", ignorerepeated=true), OK | DELIMITED | NEWLINE | EOF),
    ("a\n", InlineString7("a"), NamedTuple(), OK | NEWLINE | EOF),
    ("a\r", InlineString7("a"), NamedTuple(), OK | NEWLINE | EOF),
    ("a\r\n", InlineString7("a"), NamedTuple(), OK | NEWLINE | EOF),
    ("abcdefg", InlineString7("abcdefg"), (; delim=nothing), OK | EOF),
    ("", InlineString7(), (; sentinel=missing), SENTINEL | EOF),
    ("{abc } xyz", InlineString7("abc "), (; openquotechar='{', closequotechar='}'), OK | QUOTED | EOF | INVALID_DELIMITER),
]

for (i, case) in enumerate(testcases)
    println("testing case = $i")
    buf, check, opts, checkcode = case
    res = Parsers.xparse(InlineString7, buf; opts...)
    # in Parsers.jl pre-v2.5, we failed to include the string value on INVALID
    @test check === res.val || (i == 14 && res.val === InlineString7(""))
    if Parsers.ok(checkcode) && Parsers.delimited(checkcode) && !Parsers.newline(checkcode)
        # due to a Parsers.jl bug in pre-v2.5, String parsing inaccurately included the
        # EOF code when it shouldn't have; so we allow either in our tests.
        code = res.code & ~EOF
    elseif i == 11
        # due to a Parsers.jl bug in pre-v2.5, String parsing failed to include the
        # ESCAPED_STRING code when it should have
        code = res.code | ESCAPED_STRING
    else
        code = res.code
    end
    @test checkcode == code
end

res = Parsers.xparse(InlineString1, "")
@test !Parsers.overflow(res.code)
res = Parsers.xparse(InlineString1, "ab")
@test Parsers.overflow(res.code)
res = Parsers.xparse(InlineString1, "b")
@test res.val === InlineString("b")

# Parse to a `Result{Any}`
# https://github.com/JuliaData/CSV.jl/issues/1033
buf = b"abc"
pos = 1
len = length(buf)
opts = Parsers.Options()
res = Parsers.xparse(InlineString7, buf, pos, len, opts, Any)
@test res isa Parsers.Result{Any}
@test res.val == "abc"

end # @testset
end # Parsers 2 xparse API

@testset "InlineString Serialization symmetry" begin
    for str in ("",  "🍕", "a", "a"^3, "a"^7, "a"^15, "a"^31, "a"^63, "a"^127, "a"^255)
        buf = IOBuffer()
        i_str = InlineString(str)
        Serialization.serialize(buf, i_str)
        seekstart(buf)
        i_str_copy = Serialization.deserialize(buf)

        @test typeof(i_str_copy) == typeof(i_str)
        @test i_str_copy == i_str
    end
end # @testset

@testset "alias tests" begin
    @test String1 == InlineString1
    @test String3 == InlineString3
    @test String7 == InlineString7
    @test String15 == InlineString15
    @test String31 == InlineString31
    @test String63 == InlineString63
    @test String127 == InlineString127
    @test String255 == InlineString255
end

@testset "sorting tests" begin
    for nelems in (50, 100, 500, 1000, 5000, 100_000)
        for T in (String1, String3, String7, String15, String31, String63, String127, String255)
            x = [randstring(rand(0:(sizeof(T) - 1))) for _ = 1:nelems];
            y = map(T, x);
            @test sort(x) == sort(y)
            @test sort(x; rev=true) == sort(y; rev=true)
        end
    end
    x = [missing, String1("b"), String1("a")]
    sort!(x)
    @test isequal(x, ["a", "b", missing])

    for T in (String1, String3, String7, String15, String31)
        strings = [randstring(rand(0:(sizeof(T) - 1))) for _ = 1:1_000]
        append!(strings, filter(s -> ncodeunits(s) < sizeof(T), ["a\0", "a", "", "a\0\0", "b"]))
        x = Union{Missing, T}[T.(strings); missing]
        @test isequal(sort(x), Union{Missing, T}[T.(sort(strings)); missing])
        @test isequal(sort(x; rev=true), Union{Missing, T}[missing; T.(sort(strings; rev=true))])
        y = T.(strings)
        @test sortperm(y) == sortperm(strings)
        @test sort(y; by=length) == T.(sort(strings; by=length))
        @test sort(y; rev=true) == T.(sort(strings; rev=true))
        @test issorted(sort(y))
        @test sort(y) == sort(y; alg=Base.Sort.DEFAULT_STABLE) == sort(y; alg=QuickSort)
    end
end

@testset "inlinestrings" begin

    @test inlinestrings([]) == []

    x = inlinestrings("$i" for i in (1, 10, 100))
    @test eltype(x) === String3
    @test x == ["1", "10", "100"]
    @test eltype(inlinestrings([randstring(i) for i = 1:3])) === String3
    @test eltype(inlinestrings([randstring(i) for i = 1:4])) === String7
    @test eltype(inlinestrings(["a", missing, "abcd"])) === Union{String7, Missing}
    @test eltype(inlinestrings([missing, "abcd"])) === Union{String7, Missing}

    # Base.SizeUnknown() case
    t() = true
    x = inlinestrings("$i" for i in (1, 10, 100) if t())
    @test eltype(x) === String3
    @test x == ["1", "10", "100"]
    @test eltype(inlinestrings(randstring(i) for i = 1:3 if t())) === String3
    @test eltype(inlinestrings(randstring(i) for i = 1:4 if t())) === String7
    @test eltype(inlinestrings(x for x in ["a", missing, "abcd"] if t())) === Union{String7, Missing}
    @test eltype(inlinestrings(x for x in [missing, "abcd"] if t())) === Union{String7, Missing}

    x = [randstring(i) for i = 1:31]
    @test InlineString.(x) == map(InlineString, x) == collect(InlineString, x)
    @test eltype(InlineString.(x)) == eltype(map(InlineString, x)) == eltype(collect(InlineString, x)) == InlineString31

    x = reshape(Union{Missing, String}["a", missing, "bbb", "cccc"], 2, 2)
    for y in (InlineString.(x), map(InlineString, x), collect(InlineString, x))
        @test size(y) == size(x)
        @test isequal(y, x)
        @test eltype(y) == Union{Missing, String7}
    end

    # promote all the way to String
    x = inlinestrings(randstring(i) for i = 1:256)
    @test eltype(x) === String
    x = inlinestrings(i == 1 ? missing : randstring(i) for i = 1:256 if t())
    @test eltype(x) === Union{Missing, String}

    # https://github.com/JuliaStrings/InlineStrings.jl/issues/25
    @test inlinestrings(fill("a", 100_000)) isa Vector{String1}
    # https://github.com/JuliaStrings/InlineStrings.jl/issues/34
    @test inlinestrings([missing, "e"]) isa Vector{Union{Missing, String1}}
end

@testset "reverse" begin
    words = split(read(joinpath(dirname(pathof(InlineStrings)), "../test/utf8.txt"), String); keepempty=false)
    for x in words
        @test InlineString(x) == String(x)
    end
end

@testset "macros" begin
    x = inline"This is a macro test"
    @test String(x) == "This is a macro test"
    @test typeof(x) == String31
    @test typeof(inline1"a") == String1
    @test typeof(inline3"a") == String3
    @test typeof(inline7"a") == String7
    @test typeof(inline15"a") == String15
    @test typeof(inline31"a") == String31
    @test typeof(inline63"a") == String63
    @test typeof(inline127"a") == String127
    @test typeof(inline255"a") == String255
end

@testset "print/show/repr" begin
    s = InlineString7("abc")
    # printing
    @test "$(s)x" == "abcx"
    @test sprint(print, s) == sprint(print, String(s)) == "abc"
    # in the repl
    @test sprint(show, MIME("text/plain"), s) == sprint(show, MIME("text/plain"), String(s)) == "\"abc\""
    # repr
    @test sprint(show, s) == "String7(\"abc\")"
    @test eval(Meta.parse(repr(s))) === s

    @test repr(String31["foo", "bar"]) == "String31[\"foo\", \"bar\"]"
    @test repr(InlineString[inline1"a", inline15"a"]) == "InlineString[String1(\"a\"), String15(\"a\")]"
end

@test inlinestrings(["a", "b", ""]) == [String1("a"), String1("b"), String1("")]
@test String1("") == ""


@testset "C compatibility" begin
    for S in SUBTYPES, n in 0:(sizeof(S) - 1)
        data = randstring(n)
        str = S(data)
        @test (@ccall strlen(str::Cstring)::Csize_t) == n
        @test (@ccall strlen(str::Ptr{UInt8})::Csize_t) == n
        @test (@ccall strcmp(str::Cstring, data::Cstring)::Cint) == 0
        @test (@ccall memcmp(str::Ptr{UInt8}, data::Ptr{UInt8}, n::Csize_t)::Cint) == 0
        # the in-memory layout: codeunits, zero padding, capacity byte (0 when full)
        bytes = collect(reinterpret(NTuple{sizeof(S), UInt8}, str))
        @test bytes[1:n] == codeunits(data)
        @test all(==(0), bytes[n+1:end-1])
        @test bytes[end] == sizeof(S) - 1 - n
    end
    @test (@ccall strcmp(InlineString15("hello")::Cstring, InlineString15("world")::Cstring)::Cint) < 0
    @test (@ccall strcmp(InlineString15("test")::Cstring, InlineString15("testing")::Cstring)::Cint) < 0
    @test_throws ArgumentError Base.cconvert(Cstring, InlineString15("has\0null"))
    @test unsafe_string(Base.unsafe_convert(Ptr{UInt8}, Base.cconvert(Ptr{UInt8}, InlineString15("has\0null"))), 8) == "has\0null"
    @test reinterpret(UInt32, String3("a")) == 0x02000061
    @test reinterpret(UInt32, String3("abc")) == 0x00636261
end

# https://github.com/JuliaStrings/InlineStrings.jl/issues/90
@testset "codeunits has no dangling pointer" begin
    s = String7("abc\nss\n")
    cu = codeunits(s)
    @test !(cu isa DenseVector)
    @test cu == codeunits("abc\nss\n")
    @test length(cu) == 7 && cu[4] == 0x0a
    @test_throws BoundsError cu[8]
    @test_throws Exception pointer(cu)
    @test_throws ArgumentError pointer(s)
    @test findfirst(==(0x0a), cu) == 4
    @test (GC.@preserve s findfirst(==(0x0a), codeunits(s))) == 4
    @test readline(IOBuffer(codeunits(String7("wq")))) == "wq"
    @test readlines(IOBuffer(codeunits(s))) == ["abc", "ss"]
    @test String(cu) == "abc\nss\n"
    @test Vector{UInt8}(cu) == Vector{UInt8}("abc\nss\n")
    @test transcode(UInt16, cu) == transcode(UInt16, "abc\nss\n")
    @test sprint(write, cu) == "abc\nss\n"
end

# https://github.com/JuliaStrings/InlineStrings.jl/issues/89
@testset "matrix alignment" begin
    ref = sprint(show, MIME("text/plain"), Any["a" "b"; "c" "d"])
    @test sprint(show, MIME("text/plain"), Any["a" "b"; String31("c") "d"]) == ref
    @test sprint(show, MIME("text/plain"), Any["a" "b"; String7("c") "d"]) == ref
    @test sprint(show, MIME("text/plain"), String7["a" "b"; "c" "d"]) == replace(ref, "Matrix{Any}" => "Matrix{String7}")
    @test Base.alignment(stdout, String7("c")) == (0, 3)
end

# https://github.com/JuliaStrings/InlineStrings.jl/issues/5
@testset "map / filter / case" begin
    for S in SUBTYPES
        n = sizeof(S) - 1
        for str in (randstring(n), "", "a", first("héllo", min(5, n)), "🍕")
            ncodeunits(str) <= n || continue
            s = S(str)
            @test uppercase(s) === S(uppercase(str))
            @test lowercase(s) === S(lowercase(str))
            @test map(uppercase, s) === S(uppercase(str))
            @test filter(isuppercase, s) === S(filter(isuppercase, str))
            @test filter(c -> true, s) === s
            @test filter(c -> false, s) === S("")
            @test map(identity, s) === s
        end
    end
    # results that no longer fit fall back to a String
    s = String3("abc")
    @test map(c -> 'é', s) == "ééé"
    @test map(c -> 'é', s) isa String
    @test map(c -> 'x', s) === String3("xxx")
    @test uppercase(String3("ǆ")) == "Ǆ" # 2-byte char, 2-byte result
    @test_throws ArgumentError map(c -> 1, s)
    @test titlecase(String15("hello world")) == "Hello World"
    @test uppercase(inline"héllo wörld") === String15("HÉLLO WÖRLD")
    @test filter(isletter, inline"h1é2l3l4o5") === String15("héllo")
end

@testset "differential tests against String" begin
    Random.seed!(0)
    alphabet = ['a', 'b', 'é', '∀', '🍕', '\0', '\n', 'Z', '\xff', '\x80'] # includes invalid UTF-8
    for S in SUBTYPES, _ in 1:300
        n = rand(0:(sizeof(S) - 1))
        str = ""
        while true
            c = rand(alphabet)
            ncodeunits(str) + ncodeunits(c) <= n || break
            str *= c
        end
        str = String(str)
        s = S(str)
        @test s == str && str == s && hash(s) == hash(str) && cmp(s, str) == 0
        @test String(s) == str && (0x00 in codeunits(str) || Symbol(s) == Symbol(str)) && Vector{UInt8}(s) == Vector{UInt8}(str)
        @test ncodeunits(s) == ncodeunits(str) && length(s) == length(str) && isascii(s) == isascii(str)
        @test collect(s) == collect(str) && collect(codeunits(s)) == collect(codeunits(str))
        @test reverse(s) == reverse(str) && isvalid(s) == isvalid(str)
        @test chomp(s) == chomp(str) && chop(s) == chop(str) && strip(s) == strip(str)
        for i in 0:ncodeunits(str)+1
            @test isvalid(s, i) == isvalid(str, i) && thisind(s, i) == thisind(str, i)
            i > 0 && i <= ncodeunits(str) && @test nextind(s, i) == nextind(str, i)
            i > 0 && i <= ncodeunits(str) && isvalid(str, i) && @test s[i] == str[i]
        end
        k = rand(0:5)
        @test first(s, k) == first(str, k) && last(s, k) == last(str, k)
        @test chop(s; head=k, tail=k) == chop(str; head=k, tail=k)
        if !isempty(str)
            i = thisind(str, rand(1:ncodeunits(str)))
            j = thisind(str, rand(i:ncodeunits(str)))
            @test s[i:j] == str[i:j]
            pre = str[1:j]
            @test startswith(s, pre) && startswith(s, S(pre)) && endswith(s, str[i:end]) && endswith(s, S(str[i:end]))
            @test chopprefix(s, pre) == chopprefix(str, pre) && chopsuffix(s, str[i:end]) == chopsuffix(str, str[i:end])
            @test startswith(s, str[i:j]) == startswith(str, str[i:j])
            # Julia < 1.12 throws for some malformed `String`s here; only compare when `String` works
            ref = try findfirst(==(str[i]), str) catch; :err end
            ref !== :err && @test findfirst(==(str[i]), s) == ref
            ref = try occursin(str[i:j], str) catch; :err end
            ref !== :err && @test occursin(str[i:j], s) == ref
        end
        other = S(randstring(rand(0:(sizeof(S) - 1))))
        ostr = String(other)
        @test cmp(s, other) == cmp(str, ostr) == cmp(s, ostr) == cmp(str, other)
        @test isless(s, other) == isless(str, ostr) && (s == other) == (str == ostr)
        @test startswith(s, other) == startswith(str, ostr) && endswith(s, other) == endswith(str, ostr)
        if sizeof(S) <= 16
            @test Base.Sort.uint_unmap(S, Base.Sort.uint_map(s, Base.Order.Forward), Base.Order.Forward) === s
            @test (Base.Sort.uint_map(s, Base.Order.Forward) < Base.Sort.uint_map(other, Base.Order.Forward)) == isless(str, ostr)
        end
    end
end

@testset "search" begin
    for S in (String31, String63, String255)
        s = S("héllo wörld héllo")
        str = String(s)
        for t in ("", "h", "héllo", "wör", "d h", "xyz", "héllo wörld héllo", "o", "é")
            @test occursin(t, s) == occursin(t, str)
            @test findfirst(t, s) == findfirst(t, str)
            @test findall(t, s) == findall(t, str)
            for i in 1:ncodeunits(str)+1
                ref = try findnext(t, str, i) catch e; e end
                if ref isa Exception
                    @test_throws typeof(ref) findnext(t, s, i)
                else
                    @test findnext(t, s, i) == ref
                end
            end
            @test occursin(S(t), s) == occursin(t, str)
            @test occursin(SubString(t), s) == occursin(t, str)
        end
        @test_throws BoundsError findnext("h", s, 0)
        @test_throws BoundsError findnext("h", s, ncodeunits(s) + 2)
        @test replace(s, "héllo" => "bye") == replace(str, "héllo" => "bye")
        @test split(s, "wörld") == split(str, "wörld")
        for c in ('h', 'é', 'ö', 'd', 'z', '\x80', '🍕')
            @test findfirst(==(c), s) == findfirst(==(c), str)
            @test findfirst(isequal(c), s) == findfirst(isequal(c), str)
            for i in 1:ncodeunits(str)+1
                if isvalid(str, i) || i == ncodeunits(str) + 1
                    @test findnext(==(c), s, i) == findnext(==(c), str, i)
                else
                    @test_throws StringIndexError findnext(==(c), s, i)
                end
            end
        end
        @test_throws BoundsError findnext(==('h'), s, 0)
        @test_throws BoundsError findnext(==('h'), s, ncodeunits(s) + 2)
    end
end

@testset "Aqua" begin
    Aqua.test_all(InlineStrings)
end

include(joinpath(dirname(pathof(InlineStrings)), "../ext/tests.jl"))
