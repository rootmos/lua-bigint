local lu = require("luaunit")

local M = require("lua-bigint")

function test_foo()
    lu.assertEquals(M.foo(1,2), 3)
end

os.exit(lu.LuaUnit.run())
