local M = {}

if require("bits") == 32 then
    M.max_base = 2^15
else
    M.max_base = 2^31
end

M.default_base = M.max_base

local __fn <const> = {}
local __mt <const> = {
    __index = __fn,
}

function M.is_bignat(x)
    return getmetatable(x) == __mt
end

return setmetatable(M, {
    __call = function(N, o)
        return N.make(o)
    end,
})
