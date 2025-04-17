# lua-bigint
[![builds.sr.ht status](https://builds.sr.ht/~rootmos/lua-bigint.svg)](https://builds.sr.ht/~rootmos/lua-bigint?)

A small arbitrary precision integer Lua library,
with an [extensive test suite](tests/src) using Haskell's
[QuickCheck](https://hackage.haskell.org/package/QuickCheck)
and [HsLua](https://hackage.haskell.org/package/hslua).

## Try it out
The `run` and `repl` convenience scripts:
* download and verifies the specified in the [`deps/lua/lua.json`](deps/lua/lua.json) file,
* [builds](deps/lua/build) both 32 and 64 bit versions of Lua and choose depending on the `LUA_BITS` environment variable,
* and adds the [`src`](src) directory to Lua's `package.path`.

## Example
Running `./run examples/bignat.lua`
```lua
@include "examples/bignat.lua"
```
outputs:
```
@include "examples/bignat.output"
```
