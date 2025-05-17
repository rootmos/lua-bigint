local Bignat <const> = require("bignat")
local I <const> = require("internal")

local M = {}

if _pantry then
    M.build_info = _pantry("build-info")
end

local __fn <const> = {}
local __mt <const> = {
    __index = __fn,
}

function M.is_bigint(x)
    return getmetatable(x) == __mt
end

local function make(abs, sign)
    if abs == 0 then
        sign = 0
    end
    local q = {
        abs = abs,
        sign = sign,
    }
    return setmetatable(q, __mt)
end

function M.make(p)
    return make(Bignat.make(p), p.sign)
end
__fn.clone = M.make

__fn.digits = I.coefficients
M.digits = __fn.digits

function M.fromstring(s, from, to)
    local sign = 0
    if s:sub(1,1) == "-" then
        sign = -1
        s = s:sub(2)
    end
    local abs = Bignat.fromstring(s, from, to)
    if sign == 0 and abs > 0 then
        sign = 1
    end
    return make(abs, sign)
end

function __fn:tostring(to)
    if self.n == 0 then
        return "0"
    end

    local sign = ""
    if self.sign < 0 then
        sign = "-"
    end

    return sign .. self.abs:tostring(to)
end
M.tostring = __fn.tostring
__mt.__tostring = __fn.tostring

local maxint <const> = Bignat.maxint:tointeger()
local minint <const> = -maxint-1

function M.frominteger(n, base)
    assert(math.type(n) == "integer")

    local base = base or Bignat.default_base
    if n == 0 then
        return make(Bignat.make{base=base}, 0)
    end

    local sign = 0
    if n < 0 then
        sign = -1
        if n == minint then
            return make(Bignat.frominteger(maxint), sign) - 1
        end
        n = -n
    else
        sign = 1
    end

    return make(Bignat.frominteger(n), sign)
end

function __fn:tointeger()
    if self.abs > Bignat.maxint then
        return nil
    end

    local i = self.abs:tointeger()
    if i == nil then
        return nil
    else
        return i * self.sign
    end
end
M.tointeger = __fn.tointeger

local function binop(a, b)
    local at, bt = M.is_bigint(a), M.is_bigint(b)
    if at ~= bt then
        if bt then
            a = M.frominteger(a, b.base)
        else
            b = M.frominteger(b, a.base)
        end
    else
        if not at then
            error("bigint binary operation called with unexpected types")
        end
    end

    if a.base == b.base then
        return a, b
    elseif a.base < b.base then
        local as = Arbbase.convert(a:digits(), a.base, b.base)
        as.base = b.base
        return make(as, a.sign), b
    else
        local bs = Arbbase.convert(b:digits(), b.base, a.base)
        bs.base = a.base
        return a, make(bs, b.sign)
    end
end

local function add(asign, aabs, bsign, babs)
    if asign == bsign then
        return make(aabs + babs, asign)
    else
        local t = Bignat.compare(aabs, babs)
        if t == 0 then
            return make(Bignat.make{}, 0)
        elseif t > 0 then
            return make(aabs - babs, asign)
        else
            return make(babs - aabs, bsign)
        end
    end
end

function M.add(a, b)
    local a, b = binop(a, b)
    return add(a.sign, a.abs, b.sign, b.abs)
end
__fn.add = M.add
__mt.__add = M.add

function M.sub(a, b)
    local a, b = binop(a, b)
    return add(a.sign, a.abs, -b.sign, b.abs)
end
__fn.sub = M.sub
__mt.__sub = M.sub

function M.mul(a, b)
    local a, b = binop(a, b)
    return make(a.abs * b.abs, a.sign * b.sign)
end
__fn.mul = M.mul
__mt.__mul = M.mul

function M.neg(a)
    if not M.is_bigint(a) then
        error("bigint unary operation called with unsuitable value")
    end
    return make(a.abs, -a.sign)
end
__fn.neg = M.neg
__mt.__unm = M.neg

function M.quotrem(a, b)
    local a, b = binop(a, b)
    local q, r = Bignat.quotrem(a.abs, b.abs)
    if a.sign == 0 then
        return make(q, 0), make(r, 0)
    elseif a.sign == b.sign then
        return make(q, 1), make(r, b.sign)
    else
        return make(q, -1), make(r, a.sign)
    end
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

function M.compare(a, b)
    local a, b = binop(a, b)
    if a.sign == b.sign then
        if a.sign == 0 then
            return 0
        end

        local a, b = binop(a, b)
        local cmp = Bignat.compare(a.abs, b.abs)
        if cmp == 0 then
            return 0
        end
        if a.sign < 0 then
            return -cmp
        end
        return cmp
    elseif a.sign < b.sign then
        return -1
    else
        return 1
    end
end
__fn.compare = M.compare

function M.eq(a, b)
    if not M.is_bigint(a) or not M.is_bigint(b) then
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

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
