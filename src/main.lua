print("Hello lua-bigint world!")

local M = require("lua-bigint")
assert(M.foo(1,2) == 3)

for k, v in pairs(arg) do
    print(string.format("%d: %s", k, v))
end

local bint = require("bint")(1024)
local x = bint(1)
x = x << 128
print(x) -- outputs: 340282366920938463463374607431768211456
assert(tostring(x) == '340282366920938463463374607431768211456')
