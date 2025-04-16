local P = require"polynomial"
local A = require"arbbase"
local N = require"bignat"
local I = require"internal"

local a <const> = N.fromstring("1260257", 10, 10)
local b <const> = N.fromstring("37", 10, 10)

assert(a.base == b.base)

local base <const> = a.base
local ao, bo <const> = a.o, b.o
local an, bn <const> = a.n, b.n

local B = N.make{0,1, base=base}

local k <const> = ao + an
local l <const> = bo + bn
print(string.format("k=%d l=%d", k, l))

local function alpha(i)
    return a[an - i]
end

local q = {base=base}
local j = k - l + 1

local r = N.make{0, base=base}
for i = 0,l-2 do
    r = r + N.make{alpha(i), o=l-2-i, base=base}
end
print(string.format("r_{-1}=%s", r))

local d

local function f(x)
    local t
    r, t = N.sub(d, b*N.make{x, base=base})
    if t then
        return -1
    end

    if r < b then
        return 0
    else
        return 1
    end
end

local i = 0
while j > 0 do
    print(string.format("\ni=%d", i))
    d = B*r + N.make{alpha(i + l - 1), base=base}
    print(string.format("d_%i=%s", i, d))
    q[j] = I.binsearch(0, base-1, f)
    print(string.format("r=%s", r))
    print(string.format("q_%d=%s", j, q[j]))
    i, j = i + 1, j - 1
end

q = N.make(q)
print(q)
