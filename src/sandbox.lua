local P = require"polynomial"
local A = require"arbbase"
local N = require"bignat"

N.default_base = 10

local a = N.fromstring"870974500"
local b = N.fromstring"1203710"
-- ; 870974500 - 1203710
--             869770790

--local a = N.fromstring"10001"
--local b = N.fromstring"230"
-- ; 10001 - 230
--             9771


print(string.format("%s - %s", a, b))

local B <const> = a.base
local a = a:digits()
a.n = #a
a.o = 0
a.base = B

local ao <const>, bo <const> = a.o, b.o
local an <const>, bn <const> = a.n, b.n

local function borrow(j)
    print(string.format("borrowing from a[%d]", j))
    local k = j + 1
    while true do
        if k > an then
            error("switch sign?")
        end

        local ak = a[k]
        assert(ak >= 0)
        if ak == 0 then
            a[k] = B - 1
            k = k + 1
        else
            a[k] = ak - 1
            break
        end
    end
end

local j = bn
local i = bo + j - ao

while j > 0 do
    print()
    local bj = b[j]
    local ai = a[i] or 0
    print(string.format("i=%d j=%d", i, j))
    print(string.format("a[i]=%d b[j]=%d", ai, bj))

    if ai < bj then
        borrow(i)
        ai = ai + B
    end

    a[i] = ai - bj

    i, j = i - 1, j - 1
end
print(N(a))
