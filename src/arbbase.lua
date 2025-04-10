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

local function carry_the_one_mut(p, i, s, B)
    assert(s>=0)
    while true do
        local k = p[i] or 0
        local q, r = divrem(s + k, B)

        s = q
        p[i] = r

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

local function carry_the_one_add_multiple_of(a, k, b, B)
    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n
    local o <const>, m <const> = math.min(ao, bo), math.max(ao+an, bo+bn)
    local n <const> = m - o

    local sum = {o=o, n=n, v=a.v or b.v}
    for i = o, m do
        assert((k*(b[i - bo + 1] or 0)) >= 0)
        local s = (a[i - ao + 1] or 0) + k*(b[i - bo + 1] or 0)

        carry_the_one_mut(sum, i-o+1, s, B)
    end

    return P.make(sum)
end

local function carry_the_one_mul(a, b, B)
    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n

    if an == 0 or bn == 0 then
        return M.make{}
    end

    -- (1 + a.o - 1) + (1 + b.o - 1)
    local o <const> = ao + bo
    -- (a.n + a.o - 1) + (b.n + b.o - 1) - o + 1
    local n <const> = an + bn - 1

    local prod = {o=o, n=n, v=a.v or b.v}
    for i = 1, an do
        local ai = a[i] or 0
        -- TODO skip if ai == 0
        for j = 1, bn do
            local bj = b[j] or 0
            -- TODO skip if bj == 0

            -- (i + ao - 1) + (j + bo - 1) - o + 1
            carry_the_one_mut(prod, i+j-1, ai*bj, B)
            --prod[i + j - 1] = (prod[i + j - 1] or 0) + ai*bj
        end
    end

    return P.make(prod)
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
        assert(sum >= 0)
        assert(sum <= 2^31) -- hmm: so maybe the previous version of munch is the correct way to go
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
