-- TODO doesn't panic when bitsize is too small (try 64 and watch it fail)
local bint = require("bint")(1024)

-- Example with Ethereum's denomination exponent
local exp <const> = 18

function parse(s)
    local m = table.pack(string.match(s, "^(%d+)$"))
    if m[1] then
        return bint(m[1]) * bint.ipow(10, exp)
    end

    local m = table.pack(string.match(s, "^(%d+)%.(%d+)$"))
    if not m[1] or not m[2] then
        error(string.format("unable to parse: %s", s))
    end

    local major = bint(m[1]) * bint.ipow(10, exp)
    local decimals = #m[2]
    if decimals > exp then
        error(string.format("too many decimals: %s (%d>%d)", s, decimals, exp))
    end

    local minor = bint(m[2]) * bint.ipow(10, exp - #m[2])

    return major + minor
end

function render(native)
    local quot, rem = bint.udivmod(native, bint.ipow(10,exp))

    local decimals = tostring(rem)
    local decimals = ("0"):rep(exp - #decimals) .. decimals

    local rstrip = #decimals
    while decimals:byte(rstrip) == 48 do -- 48 == ("0"):byte(1)
        rstrip = rstrip - 1
    end
    decimals = decimals:sub(1,rstrip)

    if decimals == "" then
        return tostring(quot)
    else
        return string.format("%s.%s", quot, decimals)
    end
end

print("round-trip")
print(render(parse("123")))
print(render(parse("217774.197130849169396589")))
print(render(parse("3.0100004")))
print(render(parse("3.14")))
print(render(parse("3.0")))

print()

print("math")
print(render(parse("123") + 1))
print(render(parse("3.14")*2))
print(render(parse("217774.197130849169396589"):tdiv(2))) -- TODO metatable.__div with integers
