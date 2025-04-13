local M = {}
local P <const> = require("polynomial")
local I <const> = require("internal")

local add, mul = I.mk_add(P.make), I.mk_mul(P.make)

function M.stencil(A, B)
    local m, s = 1, A
    while s < B do
        m = m + 1
        s = s * A
    end

    local p, i = {v="B"}, 1
    while s > 0 do
        s, p[i] = divrem(s, B)
        i = i + 1
    end
    return m, P.make(p)
end

function M.convert(a, A, B)
    local m <const>, stencil <const> = M.stencil(A, B)

    local o_a, n_a <const> = 0, #a
    local M = P{1,v="B"}
    local b = P{}

    local function munch()
        local sum = 0
        local pow = 1
        for j = 1,m do
            sum = sum + (a[o_a + j] or 0)*pow
            pow = pow * A
        end
        o_a = o_a + m

        local p, i = {v="B"}, 1
        while sum > 0 do
            sum, p[i] = divrem(sum, B)
            i = i + 1
        end

        return P.make(p)
    end

    while o_a <= n_a do
        b = add(b, mul(munch(), M, B), B)
        M = mul(M, stencil, B)
    end

    return b:coefficients()
end

M.dec_to_hex = function(a) return M.convert(a, 10, 16) end
M.hex_to_dec = function(a) return M.convert(a, 16, 10) end

return M
