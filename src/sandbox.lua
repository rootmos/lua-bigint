local P = require"polynomial"
local A = require"arbbase"
local N = require"bignat"
local I = require"internal"

local y = 3
local cmp = function(x) if x == y then return 0 elseif x < y then return 1 else return -1 end end
print(I.binsearch(0, 9, cmp))
