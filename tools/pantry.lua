-- pantry.lua
-- https://git.sr.ht/~rootmos/lua-hack a09d800da1a2c2ca3f8ae827f720018e24960594
-- 2025-04-18T22:21:43+02:00 SHA-256:f3d4d00b7cd5588d31239452373ef974d4604edbdece683036f726a369efea4e
-- sed '1,4d' | sha256sum
local _pantry <const> = (function()
    local loaders = {}
    local loaded = {}

    local mt = {}
    local p = setmetatable({}, mt)

    function mt.__newindex(q, modname, def)
        assert(p == q)
        assert(type(def) == "function")
        loaders[modname] = def
    end

    function mt.__index(q, modname)
        assert(p == q)
        local M = loaded[modname]
        if M ~= nil then
            return M
        end

        f = loaders[modname]
        if f == nil then
            error(string.format("module %s not found in pantry", modname))
        end

        M = f()
        loaded[modname] = M
        return M
    end

    mt.__call = mt.__index

    return p
end)()
