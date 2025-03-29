local x = 953262827964357507520131
assert(math.type(x) == "float")

print(x) -- 9.5326282796436e+23

local exp <const> = 1e18
print(string.format("%f...ETH", x/exp)) -- 953262.827964...ETH -- here at least you indicate that the value isn't precise


-- and just for fun, I ran:
print(string.format("%f", x)) -- 953262827964357475827712.000000
-- through calc (http://www.isthe.com/chongo/tech/comp/calc/)
-- my favorite arbitrary precision calculator I use for everything these days
--
-- ; 953262827964357507520131 - 953262827964357475827712.000000
--         31692419
