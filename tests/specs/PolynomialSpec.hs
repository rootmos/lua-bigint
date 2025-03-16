module PolynomialSpec where

import System.Environment ( lookupEnv )
import Control.Monad.IO.Class ( MonadIO )

import qualified Data.ByteString as BS

import Test.Hspec
--import Test.QuickCheck

import HsLua

import LuaUtils
import Paths_lua_bigint

pickLuaSource :: IO FilePath
pickLuaSource = lookupEnv "LUA_BIGINT_SRC" >>= \case
  Just p -> return p
  Nothing -> Paths_lua_bigint.getDataFileName "lua"

prepare :: LuaError e => LuaE e ()
prepare = stackNeutral $ do
    openlibs
    src <- liftIO pickLuaSource
    extendLuaPath src

    _ <- require "polynomial"
    setglobal "P"

doRun :: (Peekable a, MonadIO m) => [BS.ByteString] -> m a
doRun ls = liftIO $ HsLua.run @HsLua.Exception $ ensureStackDiff 1 $ do
  prepare
  flip mapM_ ls $ \l -> do
    OK <- dostring l
    return ()
  peek top

spec :: Spec
spec = do
  describe "polynomial.lua" $ do
    it "should prepare properly" $ do
      t <- liftIO $ HsLua.run @HsLua.Exception $ do
        prepare
        OK <- dostring "return type(P)"
        peek @String top
      t `shouldBe` "table"

    describe "make" $ do
      describe "polynomials from inside Lua" $ do
        it "should have the correct type" $ do
          doRun [ "return P.is_polynomial(P.make{7,0,9})" ] >>= flip shouldBe True
        it "should have the correct coefficients" $ do
          doRun [ "return P.make{7,0,9}" ] >>= flip shouldBe ([7, 0, 9] :: [Int])
        it "should have the correct order" $
          doRun [ "return P.make{7,0,9}.n" ] >>= flip shouldBe (3 :: Int)
        it "should have the default offset" $
          doRun [ "return P.make{7,0,9}.o" ] >>= flip shouldBe (0 :: Int)

    --it "should have an identity element" $ property $ \(a :: Int) ->
        --a + 0 `shouldBe` a
    --it "should be commutative" $ property $ \(a :: Int) b ->
        --a + b `shouldBe` b + a
