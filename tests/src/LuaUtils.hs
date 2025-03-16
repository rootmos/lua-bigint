module LuaUtils where

import HsLua hiding ( Integer )

import Data.List ( intercalate )
import Control.Monad ( unless )
import Text.Printf ( printf )
import System.FilePath ( (</>) )

--import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as BSUTF8

prependToLuaPath :: FilePath -> String -> String
prependToLuaPath dir path =
  let d0 = dir </> "?.lua" in
  let d1 = dir </> "?" </> "init.lua" in
  intercalate ";" [d0, d1, path]

ensureStackDiff :: LuaError e => Int -> LuaE e a -> LuaE e a
ensureStackDiff d f = do
  before <- fromStackIndex <$> gettop
  a <- f
  after <- fromStackIndex <$> gettop
  let expected = fromIntegral d + before
  if after == expected then return a
  else failLua $ printf "unexpected stack differential: %d != %d" (fromIntegral @_ @Int after) (fromIntegral @_ @Int expected)

stackNeutral :: LuaError e => LuaE e a -> LuaE e a
stackNeutral f = do
  before <- fromStackIndex <$> gettop
  a <- f
  after <- fromStackIndex <$> gettop
  if after == before then return a
  else failLua $ printf "not stack-netural! %d" (fromIntegral @_ @Int $ after - before)

require :: LuaError e => String -> LuaE e StackIndex
require modname = ensureStackDiff 1 $ do
  t <- getglobal "require"
  unless (t == TypeFunction) $ throwTypeMismatchError "function" top
  pushstring $ BSUTF8.fromString modname
  call 1 1
  gettop

extendLuaPath :: LuaError e => FilePath -> LuaE e ()
extendLuaPath dir = stackNeutral $ do
  openpackage

  t <- getfield top "path"
  unless (t == TypeString) $ throwTypeMismatchError "string" top
  path <- BSUTF8.toString <$> tostring' top
  pop 2

  let path' = prependToLuaPath dir path

  pushstring $ BSUTF8.fromString path'
  setfield (nth 2) "path"
  pop 1
