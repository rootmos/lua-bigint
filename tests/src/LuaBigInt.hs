module LuaBigInt where

import System.Environment ( lookupEnv )

import HsLua hiding ( Integer )

import LuaUtils
import Paths_lua_bigint

pickLuaSource :: IO FilePath
pickLuaSource = lookupEnv "LUA_BIGINT_SRC" >>= \case
  Just p -> return p
  Nothing -> Paths_lua_bigint.getDataFileName "lua"

prepare :: LuaError e => LuaE e ()
prepare = stackNeutral $ do
  openbase >> pop 1
  openmath >> setglobal "math"
  openstring >> setglobal "string"
  opentable >> setglobal "table"

  src <- liftIO pickLuaSource
  extendLuaPath src

maxBase :: Integer
maxBase = 2^(case luaBits of { Lua32 -> 15 :: Integer; Lua64 -> 31 })
