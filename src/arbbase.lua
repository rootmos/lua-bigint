local M = {}
local P <const> = require("polynomial")

local function divrem(a, b)
    return a//b, a%b
end

function M.stencil(A, B)
    local m, s = 1, A
    while s < B do
        m = m + 1
        s = s * A
    end

    local p, i = {v="B"}, 1
    while s > 0 do
        local q, r = divrem(s, B)
        p[i] = r
        s = q
        i = i + 1
    end
    return m, P.make(p)
end

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

local function carry_the_one_add_multiple_of(a, k, b, B)
    return carry_the_one(a + P{k}*b, B)
end

local function carry_the_one_mul(a, b, B)
    return carry_the_one(a*b, B)
end

function M.convert(a, A, B)
    local m <const>, stencil <const> = M.stencil(A, B)

    local o_a, n_a <const> = 0, #a
    local M = P{1,v="B"}
    local b = P.make{v="B"}

    local function munch()
        local sum = 0
        local pow = 1
        for j = 1,m do
            sum = sum + (a[o_a + j] or 0)*pow
            pow = pow * A
        end
        o_a = o_a + m
        return sum
    end

    while o_a <= n_a do
        b = carry_the_one_add_multiple_of(b, munch(), M, B)
        M = carry_the_one_mul(M, stencil, B)
    end

    return b:coefficients()
end

M.dec_to_hex = function(a) return M.convert(a, 10, 16) end
M.hex_to_dec = function(a) return M.convert(a, 16, 10) end

return M
