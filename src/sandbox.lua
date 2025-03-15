local P = require("polynomial")

local a = {1, 2, 3, o=0, n=3}
local b = {4, nil, 5, o=1, n=3, v="y"}
local c = {7, o=8, n=1, v="z"}
local d = {9, o=0, n=1, v="A"}

print(string.format("a := %s", P.tostring(a)))
print(string.format("b := %s", P.tostring(b)))
print(string.format("c := %s", P.tostring(c)))
print(string.format("d := %s", P.tostring(d)))

print(string.format("a + b = %s", P.tostring(P.add(a, b))))
print(string.format("b + a = %s", P.tostring(P.add(b, a))))
print(string.format("b + c = %s", P.tostring(P.add(b, c))))
print(string.format("c + b = %s", P.tostring(P.add(c, b))))
