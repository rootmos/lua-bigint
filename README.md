# lua-bigint
[![Tests](https://github.com/rootmos/lua-bigint/actions/workflows/tests.yaml/badge.svg)](https://github.com/rootmos/lua-bigint/actions/workflows/tests.yaml)

A small arbitrary precision integer Lua library,
with an [extensive test suite](tests/src) using Haskell's
[QuickCheck](https://hackage.haskell.org/package/QuickCheck)
and [HsLua](https://hackage.haskell.org/package/hslua).


## Try it out
Download [bignat.lua](https://github.com/rootmos/lua-bigint/releases/download/latest/bignat.lua)
from the [latest](https://github.com/rootmos/lua-bigint/releases/latest)
and chuck into Lua:
```sh
lua -l N=bignat -e 'print(N.build_info)'
```

### Or grab the latest pre-release using [`gh`](https://cli.github.com/manual/gh_release)
```sh
gh release download "$(gh release list --json tagName --jq '.[0].tagName')" --pattern bignat.lua
```

### Or build from the source
```
./mk-dist && ls dist
```

### Or use the development scripts
The `run` and `repl` scripts:
* download and verify the Lua version specified in the [`deps/lua/lua.json`](deps/lua/lua.json) file,
* [builds](deps/lua/build) both 32 and 64 bit versions and select which depending on the `LUA_BITS` environment variable,
* and adds the [`src`](src) directory to Lua's `package.path`.

## Example
Running `./run examples/bignat.lua`
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
