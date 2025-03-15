module Main where

import HsLua

import Data.List ( intercalate )
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )
import System.IO ( hPutStr, stderr )

import Text.Printf ( printf )

--import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as BSUTF8

import Paths_lua_bigint

prependToLuaPath :: FilePath -> String -> String
prependToLuaPath dir path =
  let d0 = dir </> "?.lua" in
  let d1 = dir </> "?" </> "init.lua" in
  intercalate ";" [d0, d1, path]

stackNeutral :: LuaError e => LuaE e a -> LuaE e a
stackNeutral f = do
  before <- fromStackIndex <$> gettop
  a <- f
  after <- fromStackIndex <$> gettop
  if after == before then return a
  else failLua $ printf "not stack-netural! %d" ((fromIntegral $ after - before) :: Int)

extendLuaPath :: LuaError e => FilePath -> LuaE e ()
extendLuaPath dir = stackNeutral $ do
  openpackage

  TypeString <- getfield top "path"
  path <- BSUTF8.toString <$> tostring' top
  pop 2

  let path' = prependToLuaPath dir path

  pushstring $ BSUTF8.fromString path'
  setfield (nth 2) "path"
  pop 1

main :: IO ()
main = do
  p <- Paths_lua_bigint.getDataFileName "lua"
  putStrLn p

  st <- HsLua.run @HsLua.Exception $ do
    openlibs
    extendLuaPath p
    dostring "require'sandbox'"
  case st of
    OK -> return ()
    _ -> do
      hPutStr stderr $ "lua error: " ++ show st ++ "\n"
      exitFailure
