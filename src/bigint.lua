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
        sign = p.sign or 1,
    }
    return setmetatable(q, __mt)
end
__fn.clone = M.make

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
