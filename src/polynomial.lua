local a = {1, 2, 3, o=0, n=3}
local b = {4, nil, 5, o=1, n=3, v="y"}
local c = {7, o=8, n=1, v="z"}
local d = {9, o=0, n=1, v="A"}

local space = " "

local function tostring(p)
    local s = ""
    local v = p.v or "x"
    for i = 1, p.n do
        local k = p[i]
        if k and k ~= 0 then
            if s ~= "" then
                s = s .. string.format("%s+%s", space, space)
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

    return string.format("[%s]%s", v, space) .. s
end

local function add(a, b)
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

print(string.format("a%s:=%s%s", space, space, tostring(a)))
print(string.format("b%s:=%s%s", space, space, tostring(b)))
print(string.format("c%s:=%s%s", space, space, tostring(c)))
print(string.format("d%s:=%s%s", space, space, tostring(d)))

print(string.format("a%s+%sb%s=%s%s", space, space, space, space, tostring(add(a, b))))
print(string.format("b%s+%sa%s=%s%s", space, space, space, space, tostring(add(b, a))))
print(string.format("b%s+%sc%s=%s%s", space, space, space, space, tostring(add(b, c))))
print(string.format("c%s+%sb%s=%s%s", space, space, space, space, tostring(add(c, b))))
