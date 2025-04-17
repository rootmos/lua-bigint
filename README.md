# lua-bigint
[![builds.sr.ht status](https://builds.sr.ht/~rootmos/lua-bigint.svg)](https://builds.sr.ht/~rootmos/lua-bigint?)

A small arbitrary precision integer Lua library,
with an extensive test suite using Haskell's
[QuickCheck](https://hackage.haskell.org/package/QuickCheck)
and [HsLua](https://hackage.haskell.org/package/hslua).

## Example
```lua
local N = require("bignat")

local a = N.fromstring("78840069980359889583804398923873743190317791505792283599345825435746711")
local b = N.fromstring("6952646245343968493900761108507")
local c = N.fromstring("8177365527942")
local d = N.fromstring("364781755081040559817863568837659573725854950265808819869491980")
local f = N.fromstring("7319870")

print((a - b)//c + d*f)
```
outputs:
```
2670155025574697618046118568329646385071709964952686972394605618273230
```
