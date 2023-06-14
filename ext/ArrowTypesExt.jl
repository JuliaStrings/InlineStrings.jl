module ArrowTypesExt

using ArrowTypes, InlineStrings

for sz in (1, 4, 8, 16, 32, 64, 128, 256)
    nm = Symbol(:InlineString, max(1, sz - 1))
    arrow_nm = Symbol("JuliaLang.InlineStrings.", nm)
    @eval begin
        ArrowTypes.arrowname(::Type{$nm}) = $(Meta.quot(arrow_nm))
        ArrowTypes.JuliaType(::Val{$(Meta.quot(arrow_nm))}) = $nm
        ArrowTypes.fromarrow(::Type{$nm}, ptr::Ptr{UInt8}, len::Int) = $nm(ptr, len)
    end
end

end