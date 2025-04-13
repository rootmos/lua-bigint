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

    local ds = Arbbase.convert(self, self.base, 10)
    return Ascii.le_digits_to_be_string(ds)
end

local addB, mulB = I.mk_add(M.make), I.mk_mul(M.make)

function M.add(a, b)
    assert(a.base == b.base)
    return addB(a, b, a.base)
end
__mt.__add = M.add

function M.mul(a, b)
    assert(a.base == b.base)
    return mulB(a, b, a.base)
end
__mt.__mul = M.mul

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
