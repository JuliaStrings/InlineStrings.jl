using InlineStrings, BenchmarkTools, Random

const types = [String1, String3, String7, String15, String31, String63, String127, String255]

ascii = [InlineString(randstring(i)) for i in map(x -> sizeof(x) - 1, types)]
str_ascii = [String(x) for x in ascii]
utf8 = [InlineString(string("π", x[3:end])) for x in ascii]
str_utf8 = [String(x) for x in utf8]

function bench()
    for (x, y) in zip(ascii, str_ascii)
        @show typeof(x), @btime codeunit($x, 1)
        @show typeof(y), @btime codeunit($y, 1)
    end

    for (x, y) in zip(utf8, str_utf8)
        @show typeof(x), @btime codeunit($x, 1)
        @show typeof(y), @btime codeunit($y, 1)
    end
end
bench()

using StatProfilerHTML
@bprofile isascii($(ascii[5]))
@bprofile isascii($(str_ascii[5]))