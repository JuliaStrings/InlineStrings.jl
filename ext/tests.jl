using Test, ArrowTypes, InlineStrings

# Exercise the ArrowTypes extension directly: Arrow.jl itself depends on
# TimeZones, whose InlineStrings compat lags, so it is tested downstream instead.
@testset "ArrowTypes extension" begin
    for T in (String1, String3, String7, String15, String31, String63, String127, String255)
        nm = ArrowTypes.arrowname(T)
        @test nm isa Symbol
        @test ArrowTypes.JuliaType(Val(nm)) === T
        s = "a"^min(3, sizeof(T) - 1)
        @test GC.@preserve s ArrowTypes.fromarrow(T, pointer(s), sizeof(s)) === T(s)
        @test ArrowTypes.toarrow(T(s)) == s
    end
end
