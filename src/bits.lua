local f = function(x) return x end

if arg and arg[1] == "-p" then
    f = function(x) print(x) end
end

local intmax = 0x7fffffffffffffff
if math.type(intmax) == "integer" and intmax + 1 < 0 then
    return f(64)
end

intmax = 0x7fffffff
if math.type(intmax) == "integer" and intmax + 1 < 0 then
    return f(32)
end

error("unknown integer width")
