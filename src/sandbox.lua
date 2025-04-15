local P = require"polynomial"
local A = require"arbbase"
local N = require"bignat"

local a, b = N.fromstring"36413", N.fromstring"36426"
print(a, b)
print(a - b)
