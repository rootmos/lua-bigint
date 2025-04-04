local intmax = 0x7fffffffffffffff
if math.type(intmax) == "integer" and intmax + 1 < 0 then
    return print(64)
end

intmax = 0x7fffffff
if math.type(intmax) == "integer" and intmax + 1 < 0 then
    return print(32)
end

error("unknown integer width")
