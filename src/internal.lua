local M = {}

function divrem(a, b)
    return a//b, a%b
end

function M.clean(p, q)
    local n <const> = p.n or #p

    local mut <const> = p == q

    local i, j = 1, 1
    while i <= n do
        if j == 1 then -- strip leading zeroes/nils
            local k <const> = p[i]
            if k == nil or k == 0 then
                q.o = q.o + 1
            else
                if not mut or i ~= j then
                    q[j] = k
                end
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
                if not mut or i ~= j then
                    q[j] = k
                end
                i = i + 1
                j = j + 1
            end
        end
    end

    if j == 1 then
        q.o = 0
    end
    q.n = j - 1
end

function M:coefficients()
    local cs = {}
    local o <const>, n <const> = self.o, self.n
    for i = 0,o-1 do
        cs[i+1] = 0
    end
    for i = 1,n do
        cs[o+i] = self[i]
    end
    return cs, o + n
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

function M.binsearch(a, b, cmp)
    while true do
        local d = b - a

        local guess = d//2 + a

        local c = cmp(guess)
        if c == 0 then
            return guess
        end

        if d == 0 then
            return nil
        end

        if c < 0 then
            b = guess - 1
        else
            a = guess + 1
        end
    end
end

return M
