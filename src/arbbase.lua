local P <const> = require("polynomial")
local Ascii <const> = require("ascii")

local M = {}

local function divrem(a, b)
    return a//b, a%b
end

-- TODO: mutate p
local function carry_the_one(p, B)
    local p = p:clone()
    local i = 1
    while true do
        local k = p[i]
        if k == nil then
            break
        end
        if k >= B then
            local q, r = divrem(k, B)
            p[i] = r
            p[i+1] = (p[i+1] or 0) + q
        end

        i = i + 1
    end
    p.n = i
    return p:clone()
end

function M.dec_to_hex(a)
    local A <const>, B <const> = 10, 16
    local stencil <const> = P.make{4, 6, v="B"}
    assert(stencil.n == 2)

    local o_a, n_a <const> = 0, #a
    local M = P{1,v="B"}
    local b = P.make{v="B"}

    local function munch()
        local sum = 0
        local pow = 1
        for j = 1,stencil.n do
            sum = sum + (a[o_a + j] or 0)*pow
            pow = pow * A
        end
        return sum
    end

    while o_a <= n_a do
        local m = munch()
        local q, r = divrem(m, B)
        local d = carry_the_one(M*P.make{r, q, v="B"}, B)
        b = carry_the_one(b + d, B)
        M = carry_the_one(M * stencil, B)
        o_a = o_a + stencil.n
    end

    return b:coefficients()
end

function M.hex_to_dec(a)
    local A <const>, B <const> = 16, 10
    local stencil <const> = P.make{6, 1, v="B"}
    assert(stencil.n == 2)

    local M = P{1,v="B"}
    local b = P.make{v="B"}

    local o_a, n_a = 0, #a
    while o_a <= n_a do
        local m = a[o_a + 1] or 0
        local q, r = divrem(m, B)
        local d = carry_the_one(M*P.make{r, q, v="B"}, B)
        b = carry_the_one(b + d, B)
        M = carry_the_one(M * stencil, B)
        o_a = o_a + 1
    end

    return b:coefficients()
end

return M
