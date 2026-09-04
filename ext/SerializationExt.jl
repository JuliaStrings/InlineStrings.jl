module SerializationExt

using Serialization, InlineStrings

# `write(io, ::InlineString)` writes the codeunits (like `String`), so the raw
# fixed-size bits have to be (de)serialized explicitly.
function Serialization.serialize(s::AbstractSerializer, x::T) where {T <: InlineString}
    Serialization.serialize_type(s, T)
    ref = Ref(x)
    GC.@preserve ref unsafe_write(s.io, Ptr{UInt8}(pointer_from_objref(ref)), sizeof(T))
    return nothing
end

Serialization.deserialize(s::AbstractSerializer, ::Type{T}) where {T <: InlineString} =
    read!(s.io, Ref{T}())[]

end
