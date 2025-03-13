local a = {1, 2, 3, o=0, n=3}
local b = {4, nil, 5, o=1, n=3, v="y"}
local c = {7, o=8, n=1, v="z"}

local function tostring(p)
    local s = ""
    local v = p.v or "x"
    for i = 1, p.n do
        local k = p[i]
        if k and k ~= 0 then
            if s ~= "" then
                s = s .. " + "
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

    return s
end

local function add(a, b)
    local ao <const>, bo <const> = a.o, b.o
    local o <const>, m <const> = math.min(ao, bo), math.max(ao+a.n, bo+b.n)
    local n <const> = m - o

    local sum = {n=n, o=o, v=a.v or b.v}
    for i = 0, n do
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

print(string.format("a := %s", tostring(a)))
print(string.format("b := %s", tostring(b)))
print(string.format("c := %s", tostring(c)))

print(string.format("a + b = %s", tostring(add(a, b))))
print(string.format("b + a = %s", tostring(add(b, a))))
print(string.format("b + c = %s", tostring(add(b, c))))
print(string.format("c + b = %s", tostring(add(c, b))))
