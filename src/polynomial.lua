-- p(x) := \sum_{k=0}^K p_k x^k = p_0 + p_1 x + ... + p_K x^K
-- local p = make{p_0, p_1, ..., p_K}
--
-- p.o := min k such that p_k ~= 0
-- p.n := max (k+1) such that p_k ~= 0
-- p_k == p[k - p.o + 1], 0 <= k <= K
-- p[i] == p_{i + p.o - 1}, 1 <= i <= p.n

local I <const> = require("internal")

local M = {
    space = " ",
}

local __fn <const> = {}
local __mt <const> = {
    __index = __fn,
}

function M.is_polynomial(x)
    return getmetatable(x) == __mt
end

function M.tostring(p)
    local s = ""
    local v <const> = p.v or "x"
    for i = 1, p.n do
        local k = p[i]
        if k and k ~= 0 then
            if s ~= "" then
                s = s .. string.format("%s+%s", M.space, M.space)
            end

            local n = (p.o or 0) + i - 1

            if n == 0 then
                s = s .. string.format("%d", k)
            elseif n == 1 then
                s = s .. string.format("%d%s", k, v)
            else
                s = s .. string.format("%d%s^%d", k, v, n)
            end
        end
    end

    return string.format("[%s]%s", v, M.space) .. s
end
__mt.__tostring = M.tostring


function M.make(p)
    local q = {
        o = p.o or 0,
        v = p.v
    }

    I.clean(p, q)
    return setmetatable(q, __mt)
end
__fn.clone = M.make

function M.add(a, b)
    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n
    local o <const>, m <const> = math.min(ao, bo), math.max(ao+an, bo+bn)
    local n <const> = m - o

    local sum = {o=o, n=n, v=a.v or b.v}
    for i = o, m do
        local k = 0

        local a_i = a[i - ao + 1]
        if a_i ~= nil then
            k = k + a_i
        end

        local b_i = b[i - bo + 1]
        if b_i ~= nil then
            k = k + b_i
        end

        if k ~= 0 then
            sum[i-o+1] = k
        end
    end

    I.clean(sum, sum)
    return setmetatable(sum, __mt)
end
__mt.__add = M.add

function M.mul(a, b)
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
            prod[i + j - 1] = (prod[i + j - 1] or 0) + ai*bj
        end
    end

    -- TODO is I.clean(prod, prod) necessary?
    return setmetatable(prod, __mt)
end
__mt.__mul = M.mul

__fn.coefficients = I.coefficients
M.coefficients = __fn.coefficients

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
