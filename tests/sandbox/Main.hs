module Main where

import HsLua hiding ( Integer )

import Control.Monad ( unless )
import System.Exit ( exitFailure )
import System.IO ( hPutStr, stderr )

import LuaUtils

import Paths_lua_bigint

makePolynomial :: (LuaError e, Integral i) => [i] -> LuaE e ()
makePolynomial coeff = ensureStackDiff 1 $ do
  polynomial <- require "polynomial"
  t <- getfield polynomial "make"
  unless (t == TypeFunction) $ throwTypeMismatchError "function" top
  remove 1

  newtable
  p <- gettop

  flip mapM_ (zip coeff [1..]) $ \(a, k) -> do
    pushinteger (fromIntegral a)
    rawseti p k

  call 1 1

main :: IO ()
main = do
  luaSrc <- Paths_lua_bigint.getDataFileName "lua"

  res <- HsLua.runEither @HsLua.Exception $ do
    openlibs
    extendLuaPath luaSrc

    stackNeutral $ do
      makePolynomial ([7, 0, 9] :: [Int])
      setglobal "a"

    _ <- dostring "return a.n"
    peek @Integer top >>= liftIO . putStrLn . show

    return ()
  case res of
    Right () -> return ()
    Left e -> do
      hPutStr stderr $ "lua error: " ++ show e ++ "\n"
      exitFailure
