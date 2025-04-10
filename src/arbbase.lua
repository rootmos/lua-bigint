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
        s, p[i] = divrem(s, B)
        i = i + 1
    end
    return m, P.make(p)
end

local function carry_the_one(p, i, s, B)
    while true do
        local k = p[i] or 0
        s, p[i] = divrem(s + k, B)

        if s > 0 then
            i = i + 1
        else
            if i > p.n then
                p.n = i
            end
            return
        end
    end
end

local function add(a, b, B)
    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n
    local o <const>, m <const> = math.min(ao, bo), math.max(ao+an, bo+bn)
    local n <const> = m - o

    local sum = {o=o, n=n, v=a.v or b.v}
    for i = o, m do
        local s = (a[i - ao + 1] or 0) + (b[i - bo + 1] or 0)
        carry_the_one(sum, i-o+1, s, B)
    end

    return P.make(sum)
end

local function mul(a, b, B)
    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n

    if an == 0 or bn == 0 then
        return P.make{}
    end

    -- (1 + a.o - 1) + (1 + b.o - 1)
    local o <const> = ao + bo
    -- (a.n + a.o - 1) + (b.n + b.o - 1) - o + 1
    local n <const> = an + bn - 1

    local prod = {o=o, n=n, v=a.v or b.v}
    for i = 1, an do
        local ai = a[i] or 0
        for j = 1, bn do
            local bj = b[j] or 0
            -- (i + ao - 1) + (j + bo - 1) - o + 1
            carry_the_one(prod, i+j-1, ai*bj, B)
        end
    end

    return P.make(prod)
end

function M.convert(a, A, B)
    local m <const>, stencil <const> = M.stencil(A, B)

    local o_a, n_a <const> = 0, #a
    local M = P{1,v="B"}
    local b = P{v="B"}

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
