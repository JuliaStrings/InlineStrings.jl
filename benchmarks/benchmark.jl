using InlineStrings, BenchmarkTools, Random

const types = [String1, String3, String7, String15, String31, String63, String127, String255]

ascii = [InlineString(randstring(i)) for i in map(x -> sizeof(x) - 1, types)]
str_ascii = [String(x) for x in ascii]
utf8 = [InlineString(string("π", x[3:end])) for x in ascii]
str_utf8 = [String(x) for x in utf8]

function bench(ascii, str_ascii, utf8, str_utf8)
    for (x, y) in zip(ascii, str_ascii)
        @show typeof(x), @btime reverse($x)
        @show typeof(y), @btime reverse($y)
    end

    for (x, y) in zip(utf8, str_utf8)
        @show typeof(x), @btime reverse($x)
        @show typeof(y), @btime reverse($y)
    end
end
bench(ascii, str_ascii, utf8, str_utf8)
