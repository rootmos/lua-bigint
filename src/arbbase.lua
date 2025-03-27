local P = require"polynomial"

local function divrem(a, b)
    return a//b, a%b
end

local function do_divrem(a, b)
    local q, r = divrem(a, b)
    print(string.format("%d divrem %d = (%d, %d)", a, b, q, r))
    return q, r
end

local A <const>, B <const> = 10, 16

local function carry_the_one(p)
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
    return p:clone() -- TODO: should call a mutable p:clean() method
end

local a = {0, 9, 8, 7, 6, 5, 4, 3, 2, 1, v="A"}
local b = P.make{v="B"}

local stencil = P.make{4, 6, v="B"}
assert(stencil.n == 2)
print(string.format("stencil = %s", stencil))

local o_a = 1
local o_b = 1
local M = P{1,v="B"}

local function munch()
    local sum = 0
    local pow = 1
    for j = 1,stencil.n do
        sum = sum + a[o_a + j - 1]*pow
        pow = pow * A
    end
    return sum
end

local i = 0
while o_a <= #a do
    print(string.format("\ni = %d", i))
    print(string.format("M = %s", M))

    local q, r = do_divrem(munch(sum), B)
    local d = carry_the_one(M*P.make{r, q, v="B"})
    print(string.format("d = %s", d))

    for j = 1,d.n do
        local k = o_b + d.o + j - 1
        b[k] = (b[k] or 0) + d[j]
        b.n = math.max(b.n, k)
    end
    b = carry_the_one(b) -- TODO mutable
    print(string.format("b = %s", b))

    i = i + 1
    M = carry_the_one(M * stencil)
    o_a = o_a + stencil.n
    o_b = o_b + M.o
    M.o = 0
end

local alphabeth <const> = "0123456789abcdefghijklmnopqrstuvwxyz"
local function render_ascii(p)
    local s = ""
    for i = (p.n or #p),1,-1 do
        local k = p[i] + 1
        s = s .. alphabeth:sub(k, k)
    end
    return s
end

print(string.format("\n%s = 0x%s", render_ascii(a), render_ascii(b)))
