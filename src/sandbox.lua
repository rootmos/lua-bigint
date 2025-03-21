local P = require("polynomial")

local A = 10
local B = 16

local A2 = P.make{4, 6, v="B"}

print(string.format("A^2 := %s", P.tostring(A2)))

local A4 = A2*A2
print(string.format("A^4 := %s", P.tostring(A4)))

local function carry_the_one(p)
    local i = 1
    while true do
        local k = p[i]
        if k == nil then
            break
        end
        if k >= B then
            local q = k // B
            local r = k % B
            p[i] = r
            p[i+1] = (p[i+1] or 0) + q
        end

        i = i + 1
    end
    p.n = i
    return p
end

-- TODO local A4_p = carry_the_one(A4:clone())
local A4_p = carry_the_one(A4)

print(string.format("A^4' := %s", P.tostring(A4)))
