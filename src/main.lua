print("Hello lua-bigint world!")

local M = require("lua-bigint")
assert(M.foo(1,2) == 3)

for k, v in pairs(arg) do
    print(string.format("%d: %s", k, v))
end
