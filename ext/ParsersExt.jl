module ParsersExt
using Parsers
using InlineStrings: InlineString, addcodeunit

Parsers.xparse(::Type{T}, buf::AbstractString, pos, len, options, ::Type{S}=T) where {T <: InlineString, S} =
    Parsers.xparse(T, codeunits(buf), pos, len, options, S)

function Parsers.xparse(::Type{T}, source::Union{AbstractVector{UInt8}, IO}, pos, len, options::Parsers.Options, ::Type{S}=T) where {T <: InlineString, S}
    res = Parsers.xparse(String, source, pos, len, options, PosLen)
    code = res.code
    overflowed = false
    poslen = res.val
    if !Parsers.valueok(code) || Parsers.sentinel(code)
        x = T()
    else
        poslen = res.val
        if Parsers.escapedstring(code) || !(source isa AbstractVector{UInt8})
            if poslen.len > (sizeof(T) - 1)
                overflowed = true
                x = T()
            else
                # manually build up InlineString
                i = poslen.pos
                maxi = i + poslen.len
                x = T()
                Parsers.fastseek!(source, i - 1)
                while i < maxi
                    b = Parsers.peekbyte(source, i)
                    if b == options.e
                        i += 1
                        Parsers.incr!(source)
                        b = Parsers.peekbyte(source, i)
                    end
                    x, overflowed = addcodeunit(x, b)
                    i += 1
                    Parsers.incr!(source)
                end
                Parsers.fastseek!(source, maxi)
            end
        else
            vlen = poslen.len
            if vlen > (sizeof(T) - 1)
                # @show T, vlen, sizeof(T)
                overflowed = true
                x = T()
            else
                # @show poslen.pos, vlen
                x = T(source, poslen.pos, vlen)
            end
        end
    end
    if overflowed
        code |= Parsers.OVERFLOW
    end
    return Parsers.Result{S}(code, res.tlen, x)
end

end
