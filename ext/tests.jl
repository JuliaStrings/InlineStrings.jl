using Test, ArrowTypes, InlineStrings

# Exercise the ArrowTypes extension directly. Full Arrow.jl integration must wait
# for TimeZones.jl to accept InlineStrings 2.
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
