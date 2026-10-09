using Dates

function parse_outcome(f, T, value; kwargs...)
    try
        return (value=f(T, value; kwargs...),)
    catch error
        return (type=typeof(error), message=sprint(showerror, error))
    end
end

function sum_parsed_ints(values)
    total = 0
    for value in values
        total += Parsers.parse(Int, value)
    end
    return total
end

@testset "Parsers 3 InlineString inputs" begin
    cases = (
        (Int, ("0", "-7", " +42 ", "9223372036854775808", "1x", "1\0", ""), (;)),
        (Int, ("ff", "FF", "fg", "-80"), (; base=16)),
        (Int, ("1,234", "12,,34", "1,23x"), (; groupmark=',')),
        (UInt8, ("255", "256", "-1", "+1", "0xff"), (;)),
        (Float64, ("0.25", "-0.0", "1e999", "5e-324", "NaN", "Inf", "0x1.8p1", "1\0"), (;)),
        (Float32, ("0.1", "1e39", "x"), (;)),
        (Float16, ("0.1", "1e9", "x"), (;)),
        (Float64, ("1.234,5", "1,2,3", "1.2,3"), (; decimal=',', groupmark='.')),
        (Bool, ("true", "false", "0", "1", "2", "TRUE", "true\0"), (;)),
        (Bool, ("yes", "no", "true", "yes!"), (; trues=("yes",), falses=("no",))),
        (BigInt, ("0", "-42", "340282366920938463463374607431768211456", "x"), (;)),
        (BigFloat, ("0.25", "-0.0", "0x1.4p1", "NaN", "x"), (;)),
        (Base.UUID, ("123e4567-e89b-12d3-a456-426614174000", "123e4567-e89b-12d3-a456-42661417400x"), (;)),
        (Date, ("2026-09-17", "2000-02-29", "2025-02-29", "2026-13-01", "x"), (;)),
        (Date, ("17/09/2026", "29/02/2000", "29/02/2025", "17-09-2026"), (; dateformat="dd/mm/yyyy")),
        (Date, ("17/09/2026", "29/02/2025", "17-09-2026"), (; dateformat=dateformat"dd/mm/yyyy")),
        (DateTime, ("2026-09-17T12:34:56.789", "2025-02-29T12:34:56", "2026-09-17T25:00:00"), (;)),
        (DateTime, ("17/09/2026 12:34:56", "29/02/2025 12:34:56"), (; dateformat=dateformat"dd/mm/yyyy HH:MM:SS")),
        (Time, ("12:34:56.789", "25:00:00", "12:60:00"), (;)),
    )
    for S in SUBTYPES, (T, texts, options) in cases, text in texts
        ncodeunits(text) < sizeof(S) || continue
        value = S(text)
        for f in (Parsers.parse, Parsers.tryparse)
            @test isequal(parse_outcome(f, T, value; options...),
                          parse_outcome(f, T, text; options...))
        end
    end
    for S in SUBTYPES
        for text in (String([0xff]), "é")
            ncodeunits(text) < sizeof(S) || continue
            for T in (Int, Float64, Bool)
                @test parse_outcome(Parsers.parse, T, S(text)) ==
                      parse_outcome(Parsers.parse, T, text)
                @test Parsers.tryparse(T, S(text)) === nothing
            end
        end
        values = S.(repeat(["0", "1", "2", "3"], 64))
        @test sum_parsed_ints(values) == 384
        sum_parsed_ints(values)
        @test (@allocated sum_parsed_ints(values)) == 0
    end
    for S in (String7, String31, String255)
        source = S("x123,y")
        bytes = codeunits(source)
        @test Parsers.parse(Int, bytes, 2, 4) == 123
        @test Parsers.tryparse(Int, bytes, 2, 5) === nothing
        @test Parsers.parsenext(Int, bytes, 2, 6) == (123, 5, Parsers.RC_OK)
        @test_throws BoundsError Parsers.parse(Int, bytes, 0, 4)
        @test_throws BoundsError Parsers.tryparse(Int, bytes, 2, 7)
        @test_throws MethodError Parsers.parse(Int, source, 2, 4)
        @test_throws MethodError Parsers.tryparse(Int, source, 2, 4)
    end
end
