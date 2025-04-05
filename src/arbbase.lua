local M = {}
local P <const> = require("polynomial")

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
        local q, r = divrem(munch(), B)
        local d = carry_the_one(M*P.make{r, q, v="B"}, B)
        b = carry_the_one(b + d, B)
        M = carry_the_one(M * stencil, B)
    end

    return b:coefficients()
end

M.dec_to_hex = function(a) return M.convert(a, 10, 16) end
M.hex_to_dec = function(a) return M.convert(a, 16, 10) end

return M
