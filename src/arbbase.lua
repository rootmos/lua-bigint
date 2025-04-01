local P <const> = require("polynomial")
local Ascii <const> = require("ascii")

local M = {}

local function divrem(a, b)
    return a//b, a%b
end

local A <const>, B <const> = 10, 16

local function carry_the_one(p)
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
    return p:clone() -- TODO: should call a mutable p:clean() method
end

function M.to_hex(a)
    local b = P.make{v="B"}

    local stencil = P.make{4, 6, v="B"}
    assert(stencil.n == 2)

    local o_a = 0
    local M = P{1,v="B"}

    local function munch()
        local sum = 0
        local pow = 1
        for j = 1,stencil.n do
            sum = sum + (a[o_a + j] or 0)*pow
            pow = pow * A
        end
        return sum
    end

    while o_a <= #a do
        local m = munch()
        local q, r = divrem(m, B)
        local d = carry_the_one(M*P.make{r, q, v="B"})
        b = carry_the_one(b + d) -- TODO mutable

        M = carry_the_one(M * stencil)
        o_a = o_a + stencil.n
    end

    return b:coefficients()
end

return M
