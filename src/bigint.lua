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
__mt.__add = M.add

function M.sub(a, b)
    local a, b = binop(a, b)
    return add(a.sign, a.abs, -b.sign, b.abs)
end
__mt.__sub = M.sub

function M.compare(a, b)
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

function __mt.__eq(a, b)
    if not M.is_bigint(a) or not M.is_bigint(b) then
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

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
