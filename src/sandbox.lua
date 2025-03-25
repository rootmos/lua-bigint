local P = require"polynomial"

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

local p = A2 * P.make{7, 1, v="B"}
print(string.format("p := %s", P.tostring(p)))
carry_the_one(p)
print(string.format("p' := %s", P.tostring(p)))

local q = P.make{13, 2} + p
print(string.format("q := %s", P.tostring(q)))
carry_the_one(q)
print(string.format("q' := %s", P.tostring(q)))

local function divrem(a, b)
    return a//b, a%b
end

local function do_divrem(a, b)
    print(string.format("%d divrem %d = (%d, %d)", a, b, divrem(a, b)))
end

do_divrem(56, B)
print(carry_the_one(P.make{1, 7, 2, v="B"}*P.make{8, 3}))

print(carry_the_one(P{1, 7, 2, v="B"}*P{4, 6}))
print(carry_the_one(A2*A2*A2))

do_divrem(34, B)

print(carry_the_one(P{4, 2, 4, 15, v="B"}*P{2, 2}))

print(carry_the_one(A2*A2*A2*A2))

print(carry_the_one(P{1, 14, 5, 15, 5, v="B"}*P{12}))


print("----")
do_divrem(90, B)
do_divrem(78, B)
print(carry_the_one(A2*P{14, 4}))
print(carry_the_one(P{10,5,v="B"}+A2*P{14, 4}))
