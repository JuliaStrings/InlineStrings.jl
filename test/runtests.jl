using Test, InlineStrings, Parsers, Serialization, Random
import Parsers: SENTINEL, OK, EOF, OVERFLOW, QUOTED, DELIMITED, INVALID_DELIMITER, INVALID_QUOTED_FIELD, ESCAPED_STRING, NEWLINE, SUCCESS

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
        io = IOBuffer()
        write(io, x)
        seekstart(io)
        @test read(io, typeof(x)) === x
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
        @test unsafe_string(Base.unsafe_convert(Ptr{UInt8}, Base.cconvert(Ptr{UInt8}, x))) == y
        @test unsafe_string(Base.unsafe_convert(Ptr{Int8}, Base.cconvert(Ptr{Int8}, x))) == y
        @test unsafe_string(Base.unsafe_convert(Cstring, Base.cconvert(Cstring, x))) == y
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

@testset "InlineString parsing" begin
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

# only test package extension on >= 1.9.0
if VERSION >= v"1.9.0" && Sys.WORD_SIZE == 64
include(joinpath(dirname(pathof(InlineStrings)), "../ext/tests.jl"))
end
