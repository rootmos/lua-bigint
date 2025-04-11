module LuaBigInt where

import System.Environment ( lookupEnv )

import HsLua

import LuaUtils
import Paths_lua_bigint

pickLuaSource :: IO FilePath
pickLuaSource = lookupEnv "LUA_BIGINT_SRC" >>= \case
  Just p -> return p
  Nothing -> Paths_lua_bigint.getDataFileName "lua"

prepare :: Prepare
prepare = do
    openlibs
    src <- liftIO pickLuaSource
    extendLuaPath src
