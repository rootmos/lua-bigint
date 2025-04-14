local Ascii = require("ascii")
local P = require("polynomial")
local Arbbase = require("arbbase")
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
    local base = p.base

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

    -- TODO compare amount of trailing zeroes

    return 0
end

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
