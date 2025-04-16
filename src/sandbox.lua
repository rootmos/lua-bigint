local P = require"polynomial"
local A = require"arbbase"
local N = require"bignat"
local I = require"internal"

N.default_base = 10

local a = N.fromstring"1260257"
local b = N.fromstring"37"

assert(a.base == b.base)
local B = N.make{0,1}

assert(a.o == 0)
assert(b.o == 0)

local k = a.o + a.n
local l = b.o + b.n
print(string.format("k=%d l=%d", k, l))

local function alpha(i)
    return a[a.n - i]
end

local function beta(i)
    return b[b.n - i]
end

local q = N.make{0}
print(string.format("q_{-1}=%s", q))

local r = N.make{0}
for i = 0,l-2 do
    r = r + N.make{alpha(i), o=l-2-i}
end
print(string.format("r_{-1}=%s", r))

local i = 0
print(string.format("i=%d", i))

local di = B*r + N.make{alpha(i + l - 1)}
print(di)
