local P = require"polynomial"

local A, B = 16, 10

local stencil = P{6, 1, v="B"}
print(string.format("stencil = %s", stencil))

local function divrem(a, b)
    return a//b, a%b
end

local function carry_the_one(p)
    local p = p:clone()
    local i = 1
    while true do
        local k = p[i]
        if k == nil then
            break
        end
        if k >= B then
            local q, r = divrem(k, B)
            p[i] = r
            p[i+1] = (p[i+1] or 0) + q
        end

        i = i + 1
    end
    p.n = i
    return p:clone() -- TODO: should call a mutable p:clean() method
end


local i = 1
local M = P{1, v="B"}*stencil
print(string.format("i = %d", i))
print(string.format("M = %s", M))

local q, r = divrem(7, B)
print(carry_the_one(M*P{r, q, v="B"}))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
local q, r = divrem(15, B)
print(carry_the_one(M*P{r, q, v="B"}))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
local q, r = divrem(8, B)
print(carry_the_one(M*P{r, q, v="B"}))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
local q, r = divrem(11, B)
print(carry_the_one(M*P{r, q, v="B"}))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
local q, r = divrem(5, B)

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
local q, r = divrem(5, B)

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
local q, r = divrem(9, B)

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
local q, r = divrem(9, B)

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))

i = i + 1
M = carry_the_one(M*stencil)
print(string.format("\ni = %d", i))
print(string.format("M = %s", M))
