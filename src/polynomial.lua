local M = {
    space = " ",
}

local __fn <const> = {}
local __mt <const> = {
    __index = __fn,
}

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
    local n <const> = p.n or #p

    local q = {
        o = p.o or 0,
        v = p.v
    }

    local i, j = 1, 1
    while i <= n do
        if j == 1 then -- strip leading zeroes/nils
            local k = p[i]
            if k == nil or k == 0 then
                q.o = q.o + 1
            else
                q[j] = k
                j = j + 1
            end
            i = i + 1
        else
            local k = p[i]
            if k == nil or k == 0 then
                local i0 = i + 1
                while true do
                    if i0 > n then -- trailing zeroes/nils
                        break
                    end

                    k = p[i0]
                    if (k or 0) ~= 0 then
                        for _ = i,i0 do -- TODO make a "sparse" table?
                            q[j] = 0
                            j = j + 1
                        end
                        q[j - 1] = k
                        break
                    end

                    i0 = i0 + 1
                end
                i = i0 + 1
            else
                q[j] = k
                i = i + 1
                j = j + 1
            end
        end
    end

    if j == 1 then
        q.o = 0
    end
    q.n = j - 1

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

    return setmetatable(sum, __mt)
end
__mt.__add = M.add

function M.mul(a, b)
    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n

    if an == 0 or bn == 0 then
        return M.make{}
    end

    -- a[i] ~ x^(i + ao - 1), 1 <= i <= a.n
    -- min: o := 1 + ao - 1 + 1 + bo - 1
    -- max: n := an + ao - 1 + bn + bo - 1 - o + 1

    local o <const> = ao + bo
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

    -- TODO cleanup leading, trailing zeros

    return setmetatable(prod, __mt)
end
__mt.__mul = M.mul

function M.is_polynomial(x)
    return getmetatable(x) == __mt
end

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
