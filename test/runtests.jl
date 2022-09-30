using Test, InlineStrings, Parsers, Serialization, Random
import Parsers: SENTINEL, OK, EOF, OVERFLOW, QUOTED, DELIMITED, INVALID_DELIMITER, INVALID_QUOTED_FIELD, ESCAPED_STRING, NEWLINE, SUCCESS, peekbyte, incr!, checksentinel, checkdelim, checkcmtemptylines

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

# make sure we don't read past an end of a buffer
buf = Vector{UInt8}("hey")
x = InlineString7(buf, 1, 3)
@test x == "hey"
@test typeof(x) == InlineString7

# https://github.com/JuliaData/WeakRefStrings.jl/issues/88
@test InlineString(String1("a")) === String1("a")

# https://github.com/JuliaData/InlineStrings.jl/issues/2
x = InlineString("hey")
@test typeof(string(x)) == String

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

@test chopprefix(abc, "a") === InlineString3("bc")
@test chopprefix(abc, "bc") === abc
@test chopprefix(abc, "abc") === InlineString3("")
@test chopprefix(InlineString1("a"), "a") === InlineString3("")

@test chopsuffix(abc, "a") === abc
@test chopsuffix(abc, "bc") === InlineString3("a")
@test chopsuffix(abc, "abc") === InlineString3("")
@test chopsuffix(InlineString1("c"), "c") === InlineString3("")

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
        @test x^5 == y^5
        @test string(x) == string(y)
        @test join([x, x]) == join([y, y])
        @test reverse(x) == reverse(y)
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
    end
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
    ("\"\",", InlineString7(), NamedTuple(), OK | QUOTED | EOF | DELIMITED), # same e & cq
    ("\"\"\"\"", InlineString7("\""), NamedTuple(), OK | QUOTED | ESCAPED_STRING | EOF), # same e & cq
    ("\"\\", InlineString7(), (; escapechar=UInt8('\\')), OK | QUOTED | INVALID_QUOTED_FIELD | EOF), # \\ e, invalid quoted
    ("\"\\\"\"", InlineString7("\""), (; escapechar=UInt8('\\')), OK | QUOTED | ESCAPED_STRING | EOF), # \\ e, valid
    ("\"\"", InlineString7(), (; escapechar=UInt8('\\')), OK | QUOTED | EOF), # diff e & cq
    ("\"a", InlineString7(), NamedTuple(), OK | QUOTED | INVALID_QUOTED_FIELD | EOF), # invalid quoted
    ("\"a\"", InlineString7("a"), NamedTuple(), OK | QUOTED | EOF), # quoted
    ("\"a\" ", InlineString7("a"), NamedTuple(), OK | QUOTED | EOF), # quoted
    ("\"a\",", InlineString7("a"), NamedTuple(), OK | QUOTED | EOF | DELIMITED), # quoted
    ("a,", InlineString7("a"), NamedTuple(), OK | EOF | DELIMITED),
    ("a__", InlineString7("a"), (; delim="__"), OK | EOF | DELIMITED),
    ("a,", InlineString7("a"), (; ignorerepeated=true), OK | EOF | DELIMITED),
    ("a__", InlineString7("a"), (; delim="__", ignorerepeated=true), OK | EOF | DELIMITED),
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
    @test check === res.val
    @test checkcode == res.code
end

res = Parsers.xparse(InlineString1, "")
@test Parsers.overflow(res.code)
res = Parsers.xparse(InlineString1, "ab")
@test Parsers.overflow(res.code)
res = Parsers.xparse(InlineString1, "b")
@test res.val === InlineString("b")

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
            x = [randstring(rand(1:(max(1, sizeof(T) - 1)))) for _ = 1:nelems];
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
