local Ascii <const> = require("ascii")
local Arbbase <const> = require("arbbase")
local I <const> = require("internal")

local M = {}

if _pantry then
    M.build_info = _pantry("build-info")
end

if require("bits") == 32 then
    M.max_base = 0x8000
else
    M.max_base = 0x80000000
end
assert(math.type(M.max_base) == "integer")

M.default_base = M.max_base

local __fn <const> = {}
local __mt <const> = {
    __index = __fn,
}

function M.is_bignat(x)
    return getmetatable(x) == __mt
end

function M.make(p)
    local base = p.base or M.default_base

    assert(base >= 2)
    assert(base <= M.max_base)
    for _, d in ipairs(p) do
        assert(math.type(d) == "integer")
        assert(0 <= d)
        assert(d < base)
    end

    local q = {
        o = p.o or 0,
        base = base,
    }
    I.clean(p, q)
    return setmetatable(q, __mt)
end
__fn.clone = M.make

__fn.digits = I.coefficients
M.digits = __fn.digits

function M.fromstring(s, from, to)
    local from = from or 10
    local to = to or M.default_base
    local fs = Arbbase.convert(Ascii.be_string_to_le_digits(s), from, to)
    fs.base = to
    return M.make(fs)
end

function __fn:tostring(to)
    if self.n == 0 then
        return "0"
    end

    local ds = Arbbase.convert(self:digits(), self.base, to or 10)
    return Ascii.le_digits_to_be_string(ds)
end
M.tostring = __fn.tostring
__mt.__tostring = __fn.tostring

function M.fromhex(s)
    return M.fromstring(s, 16)
end

function __fn:tohex()
    return self:tostring(16)
end

function M.frombigendian(bs)
    local p = table.pack(string.byte(bs, 1, #bs))
    local q = {base=256}
    for i = 1,#p do
        q[#p - i + 1] = p[i]
    end
    return M.make(q)
end

function __fn:tobigendian()
    return string.reverse(self:tolittleendian())
end

function M.fromlittleendian(bs)
    local p = table.pack(string.byte(bs, 1, #bs))
    p.base = 256
    return M.make(p)
end

function __fn:tolittleendian()
    return string.char(table.unpack(Arbbase.convert(self:digits(), self.base, 256)))
end

local addB, mulB = I.mk_add(M.make), I.mk_mul(M.make)

function M.frominteger(n, base)
    assert(math.type(n) == "integer")
    if n < 0 then
        error("unexpected negative integer")
    end

    local base = base or M.default_base
    local o = {base=base}
    local i = 1
    while n > 0 do
        n, o[i] = I.divrem(n, base)
        i = i + 1
    end

    return M.make(o)
end

if require("bits") == 32 then
    M.maxint = M.frominteger(0x7fffffff)
else
    M.maxint = M.frominteger(0x7fffffffffffffff)
end

function __fn:tointeger()
    if self > M.maxint then
        return nil
    end

    local exp = 1
    for i = 1,self.o do
        exp = exp * self.base
    end

    local sum = 0
    for i = 1,self.n do
        assert(exp > 0)
        sum = sum + exp*self[i]
        exp = exp * self.base
    end
    return sum
end
M.tointeger = __fn.tointeger

local function binop(a, b)
    local at, bt = M.is_bignat(a), M.is_bignat(b)
    if at ~= bt then
        if bt then
            a = M.frominteger(a, b.base)
        else
            b = M.frominteger(b, a.base)
        end
    else
        if not at then
            error("bignat binary operation called with unexpected types")
        end
    end

    if a.base == b.base then
        return a, b
    elseif a.base < b.base then
        local as = Arbbase.convert(a:digits(), a.base, b.base)
        as.base = b.base
        return M.make(as), b
    else
        local bs = Arbbase.convert(b:digits(), b.base, a.base)
        bs.base = a.base
        return a, M.make(bs)
    end
end

function M.add(a, b)
    local a, b = binop(a, b)
    return addB(a, b, a.base)
end
__fn.add = M.add
__mt.__add = M.add

function M.mul(a, b)
    local a, b = binop(a, b)
    return mulB(a, b, a.base)
end
__fn.mul = M.mul
__mt.__mul = M.mul

function M.neg(a)
    if not M.is_bignat(a) then
        error("bignat unary operation called with unsuitable value")
    end
    return M.make{base=a.base}
end
__fn.neg = M.neg
__mt.__unm = M.neg

function M.compare(a, b)
    local a, b = binop(a, b)

    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n
    local am <const>, bm = ao + an, bo + bn

    if am < bm then
        return -1
    elseif am > bm then
        return 1
    end

    local ai, bi = an, bn

    while ai > 0 and bi > 0 do
        local ak, bk = a[ai], b[bi]
        if ak < bk then
            return -1
        elseif ak > bk then
            return 1
        end

        ai, bi = ai - 1, bi - 1
    end

    if ao < bo then
        return 1
    elseif ao > bo then
        return -1
    end
    return 0
end
__fn.compare = M.compare

function M.eq(a, b)
    if not M.is_bignat(a) or not M.is_bignat(b) then
        return false
    end

    if rawequal(a, b) then
        return true
    end

    local a, b = binop(a, b)
    return M.compare(a, b) == 0
end
__fn.eq = M.eq
__mt.__eq = M.eq

function M.neq(a, b)
    return not M.eq(a, b)
end
__fn.neq = M.neq


function M.lt(a, b)
    if rawequal(a, b) then
        return false
    end

    local a, b = binop(a, b)
    return M.compare(a, b) < 0
end
__mt.__lt = M.lt
__fn.lt = M.lt

function M.le(a, b)
    if rawequal(a, b) then
        return true
    end

    local a, b = binop(a, b)
    return M.compare(a, b) <= 0
end
__fn.le = M.le

function M.gt(a, b)
    if rawequal(a, b) then
        return false
    end

    local a, b = binop(a, b)
    return M.compare(a, b) > 0
end
__mt.__gt = M.gt
__fn.gt = M.gt

function M.ge(a, b)
    if rawequal(a, b) then
        return true
    end

    local a, b = binop(a, b)
    return M.compare(a, b) >= 0
end
__fn.ge = M.ge

function M.tsub(a, b)
    if rawequal(a, b) then
        return M.make{0}, false
    end

    local a, b = binop(a, b)
    local ao, bo <const> = a.o, b.o
    local an, bn <const> = a.n, b.n
    local am <const>, bm <const> = ao + an, bo + bn

    if am < bm then
        return M.make{0}, true
    end

    local base <const> = a.base
    local a, an = a:digits()
    a.n = an
    a.o = 0
    ao = 0
    a.base = base

    local function borrow(j)
        local k = j + 1
        while true do
            if k > an then
                return false
            end

            local ak = a[k]
            if ak == 0 then
                a[k] = base - 1
                k = k + 1
            else
                a[k] = ak - 1
                break
            end
        end
        return true
    end

    local j = bn
    local i = bo + j - ao
    while j > 0 do
        local ai, bj = a[i] or 0, b[j]

        if ai < bj then
            if not borrow(i) then
                return M.make{0}, true
            end
            ai = ai + base
        end

        a[i] = ai - bj
        i, j = i - 1, j - 1
    end

    return M.make(a), false
end
__fn.tsub = M.tsub

function M.sub(a, b)
    local d, _ = M.tsub(a, b)
    return d
end
__fn.sub = M.sub
__mt.__sub = M.sub

-- https://en.wikipedia.org/wiki/Long_division#Algorithm_for_arbitrary_base
function M.quotrem(a, b)
    local a, b = binop(a, b)

    local base <const> = a.base
    local B <const> = M.make{0,1, base=base}

    if b == M{0, base=base} then
        error("attempt to divide by zero")
    end

    -- TODO check if checking a == b significantly affects performance
    if rawequal(a, b) or a == b then
        return M{1, base=base}, M{0, base=base}
    end

    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n
    local k <const> = ao + an
    local l <const> = bo + bn

    if k < l then
        return M.make{0, base=base}, a
    end

    local function alpha(i)
        if i > an then
            return 0
        end
        return a[an - i]
    end

    local r = M.make{0, base=base}
    for i = 0,l-2 do
        r = r + M.make{alpha(i), o=l-2-i, base=base}
    end

    local d
    local function f(x)
        local t
        r, t = M.sub(d, b*M.make{x, base=base})
        if t then
            return -1
        end

        if r < b then
            return 0
        else
            return 1
        end
    end

    local q = {base=base}
    for j = (k - l + 1),1,-1 do
        d = B*r + M.make{alpha(k - j), base=base}
        q[j] = I.binsearch(0, base-1, f)
    end

    return M.make(q), r
end
__fn.quotrem = M.quotrem

function M.quot(a, b)
    local q, _ = M.quotrem(a, b)
    return q
end
__fn.quot = M.quot
__mt.__idiv = M.quot

function M.rem(a, b)
    local _, r = M.quotrem(a, b)
    return r
end
__fn.rem = M.rem
__mt.__mod = M.rem

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
