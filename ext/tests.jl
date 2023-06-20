using Test, Arrow, InlineStrings

@testset "basic Arrow.jl interop" begin
    t = (x = inlinestrings(["a", "b", "sailor"]),)
    t2 = Arrow.Table(Arrow.tobuffer(t))
    @test isequal(t.x, t2.x)
    @test t2.x[1] isa InlineString
end
