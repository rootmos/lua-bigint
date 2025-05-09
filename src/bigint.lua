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

function M.make(p)
    local q = {
        abs = Bignat.make(p),
        sign = p.sign,
    }
    return setmetatable(q, __mt)
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
    local q = {
        abs = Bignat.fromstring(s, from, to),
        sign = sign,
    }
    return setmetatable(q, __mt)
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

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
