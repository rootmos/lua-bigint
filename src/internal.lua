local M = {}

function divrem(a, b)
    return a//b, a%b
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

function M.mk_add(f)
    return function(a, b, B)
        local ao <const>, bo <const> = a.o, b.o
        local an <const>, bn <const> = a.n, b.n
        local o <const>, m <const> = math.min(ao, bo), math.max(ao+an, bo+bn)
        local n <const> = m - o

        local sum = {o=o, n=n, v=a.v or b.v, base=B}
        for i = o, m do
            local s = (a[i - ao + 1] or 0) + (b[i - bo + 1] or 0)
            carry_the_one(sum, i-o+1, s, B)
        end

        return f(sum)
    end
end

function M.mk_mul(f)
    return function(a, b, B)
        local ao <const>, bo <const> = a.o, b.o
        local an <const>, bn <const> = a.n, b.n

        if an == 0 or bn == 0 then
            return f{base=B}
        end

        -- (1 + a.o - 1) + (1 + b.o - 1)
        local o <const> = ao + bo
        -- (a.n + a.o - 1) + (b.n + b.o - 1) - o + 1
        local n <const> = an + bn - 1

        local prod = {o=o, n=n, v=a.v or b.v, base=B}
        for i = 1, an do
            local ai = a[i] or 0
            for j = 1, bn do
                local bj = b[j] or 0
                local s = ai*bj
                if s > 0 then
                    -- (i + a.o - 1) + (j + b.o - 1) - o + 1
                    carry_the_one(prod, i+j-1, s, B)
                end
            end
        end

        return f(prod)
    end
end

return M
