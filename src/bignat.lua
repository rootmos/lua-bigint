local Ascii <const> = require("ascii")
local P <const> = require("polynomial")
local Arbbase <const> = require("arbbase")
local I <const> = require("internal")

local M = {}

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
        assert(0 <= d)
        assert(d < base)
    end

    local q = {
        o = p.o or 0,
        base = base,
    }
    P.clean(p, q)
    return setmetatable(q, __mt)
end
__fn.clone = M.make

__fn.digits = P.coefficients
M.digits = __fn.digits

function M.fromstring(s, from, to)
    local from = from or 10
    local to = to or M.default_base
    local fs = Arbbase.convert(Ascii.be_string_to_le_digits(s), from, to)
    fs.base = to
    return M.make(fs)
end

function __fn:tostring()
    if self.n == 0 then
        return "0"
    end

    local ds = Arbbase.convert(self:digits(), self.base, 10)
    return Ascii.le_digits_to_be_string(ds)
end
M.tostring = __fn.tostring

local addB, mulB = I.mk_add(M.make), I.mk_mul(M.make)

local function binop(a, b)
    -- TODO promote integers
    assert(M.is_bignat(a), M.is_bignat(b)) -- TODO add tests and better error message
    assert(a.base == b.base) -- TODO or convert to max(a.base, b.base)?
    return a, b
end

function M.add(a, b)
    local a, b = binop(a, b)
    return addB(a, b, a.base)
end
__mt.__add = M.add

function M.mul(a, b)
    local a, b = binop(a, b)
    return mulB(a, b, a.base)
end
__mt.__mul = M.mul

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

function __mt.__eq(a, b)
    if not M.is_bignat(a) or not M.is_bignat(b) then
        return false
    end

    if rawequal(a, b) then
        return true
    end

    local a, b = binop(a, b)
    return M.compare(a, b) == 0
end

function __mt.__lt(a, b)
    if rawequal(a, b) then
        return false
    end

    local a, b = binop(a, b)
    return M.compare(a, b) < 0
end

function __mt.__gt(a, b)
    if rawequal(a, b) then
        return false
    end

    local a, b = binop(a, b)
    return M.compare(a, b) > 0
end

local function carry_the_negative_one(p, i, s, B)
    while i >= 1 do
        local k = p[i] or 0
        s, p[i] = divrem(k - s, B)

        if s < 0 then
            i = i - 1
        else
            if i > p.o then
                p.o = i
            end
            return
        end
    end
end

function M.sub(a, b)
    local a, b = binop(a, b)

    local ao <const>, bo <const> = a.o, b.o
    local an <const>, bn <const> = a.n, b.n
    local am <const>, bm = ao + an, bo + bn

    if am < bm then
        return M.make{0}
    end

    local diff = a:clone()

    return diff
end
__mt.__sub = M.sub

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
