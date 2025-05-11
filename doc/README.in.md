# lua-bigint
[![Tests](https://github.com/rootmos/lua-bigint/actions/workflows/tests.yaml/badge.svg)](https://github.com/rootmos/lua-bigint/actions/workflows/tests.yaml)

A small arbitrary precision integer Lua library,
with a [paranoid and extensive test suite](tests/src) using Haskell's
native arbitrary precision integers,
[QuickCheck](https://hackage.haskell.org/package/QuickCheck)
and [HsLua](https://hackage.haskell.org/package/hslua).

## Example
```lua
@include "examples/bigint.lua"
```
```
@include "examples/bigint.output"
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
