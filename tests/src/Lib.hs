module Lib where

import System.Environment ( lookupEnv )
import System.FilePath ( (</>) )

import HsLua hiding ( Integer )

import LuaUtils
import Paths_lua_bigint

pickLuaSource :: IO FilePath
pickLuaSource = lookupEnv "LUA_BIGINT_SRC" >>= \case
  Just p -> return p
  Nothing -> Paths_lua_bigint.getDataFileName "lua"

minlibs :: LuaError e => LuaE e ()
minlibs = stackNeutral $ do
  openbase >> pop 1
  openmath >> setglobal "math"
  openstring >> setglobal "string"
  opentable >> setglobal "table"

prepare :: LuaError e => LuaE e ()
prepare = stackNeutral $ do
  minlibs

  openpackage >> setglobal "package"
  liftIO pickLuaSource >>= extendLuaPath

prepare' :: LuaError e => Name -> String -> LuaE e ()
prepare' name modname = stackNeutral $ do
  minlibs

  liftIO (lookupEnv "LUA_BIGINT_DIST") >>= \case
    Nothing -> do
      openpackage >> setglobal "package"
      liftIO pickLuaSource >>= extendLuaPath
      name `requireG` modname
    Just p -> do
      OK <- dofile (Just $ p </> modname ++ ".lua")
      setglobal name

maxBase :: Integer
maxBase = 2^(case luaBits of { Lua32 -> 15 :: Integer; Lua64 -> 31 })
