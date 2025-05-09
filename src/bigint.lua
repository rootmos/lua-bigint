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
    local sign = 1
    if s:sub(1,1) == "-" then
        sign = -1
        s = s:sub(2)
    end
    return make(Bignat.fromstring(s, from, to), sign)
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

local function add(asign, aabs, bsign, babs)
    if asign == bsign then
        return make(aabs + babs, asign)
    else
        if aabs >= babs then
            return make(aabs - babs, asign)
        else
            return make(babs - aabs, bsign)
        end
    end
end

function M.add(a, b)
    return add(a.sign, a.abs, b.sign, b.abs)
end
__mt.__add = M.add

function M.sub(a, b)
    return add(a.sign, a.abs, -b.sign, b.abs)
end
__mt.__sub = M.sub

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
