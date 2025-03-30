local lu = require("luaunit")
local M = require("arbbase")
local Ascii = require("ascii")

function run_example(a0, b0)
    local b1 = M.to_hex(Ascii.be_string_to_le_digits(a0))
    local a1 = Ascii.le_digits_to_be_string(b1)
    lu.assertEquals(b0, a1)
end

function test_example0()
    run_example(
        "10000000000000000000000000000000000000000000000000000000000000000000000000000000001",
        "15159af8044462379881065d41ad19c19af0eeb576360c36400000000000000000001"
    )
end

os.exit(lu.LuaUnit.run())
