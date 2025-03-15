local M = {
    space = " ",
}

function M.tostring(p)
    local s = ""
    local v = p.v or "x"
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

function M.add(a, b)
    local ao <const>, bo <const> = a.o, b.o
    local o <const>, m <const> = math.min(ao, bo), math.max(ao+a.n, bo+b.n)
    local n <const> = m - o + 1

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
            sum[i] = k
        end
    end

    return sum
end

return M
