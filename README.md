# lua-bigint
[![Tests](https://github.com/rootmos/lua-bigint/actions/workflows/tests.yaml/badge.svg)](https://github.com/rootmos/lua-bigint/actions/workflows/tests.yaml)

A small arbitrary precision integer Lua library,
with a [paranoid and extensive test suite](tests/src) using Haskell's
native arbitrary precision integers,
[QuickCheck](https://hackage.haskell.org/package/QuickCheck)
and [HsLua](https://hackage.haskell.org/package/hslua).

## Example
```lua
local I = require("bigint")

local a = I.fromstring("78840069980359889583804398923873743190317791505792283599345825435746711")
local b = I.fromstring("6952646245343968493900761108507")
local c = I.fromstring("-8177365527942")
local d = I.fromstring("364781755081040559817863568837659573725854950265808819869491980")
local f = 7319870

print((a - b - 1)//(c + 1) + (d % f))
```
```
-9641255452134745719230413362696917903840332893074689350422
```

## Try it out
Download [bigint.lua](https://github.com/rootmos/lua-bigint/releases/latest/download/bigint.lua)
from the [latest](https://github.com/rootmos/lua-bigint/releases/latest) release
and chuck it into Lua:
```sh
lua -l I=bigint -e 'print(I.build_info)'
```

### Or grab the latest pre-release using [`gh`](https://cli.github.com/manual/gh_release)
```sh
gh release download "$(gh release list --json tagName --jq '.[0].tagName')" --pattern bigint.lua
```

### Or build from the source
```
./mk-dist && ls dist
```

### Or use the development scripts
The [`run`](run) and [`repl`](repl) scripts:
* download and verify the Lua version specified in the [`deps/lua/lua.json`](deps/lua/lua.json) file,
* [builds](deps/lua/build) both 32 and 64 bit versions and select which depending on the `LUA_BITS` environment variable,
* and adds the [`src`](src) directory to Lua's `package.path`.
