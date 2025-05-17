module LuaUtilsSpec where

import System.Environment ( lookupEnv )
import Data.Maybe ( fromMaybe )

import Test.Hspec

import HsLua hiding ( Integer, error )

import LuaUtils

expectedBits :: IO (Maybe LuaBits)
expectedBits = f <$> lookupEnv "LUA_BITS"
  where f (Just "32") = Just Lua32
        f (Just "64") = Just Lua64
        f (Just "") = Nothing
        f Nothing = Nothing
        f (Just s) = error $ "unable to parse LUA_BITS: " ++ s

runLua :: RunLuaRun
runLua = mkRun openlibs

spec :: Spec
spec = describe "LuaUtils" $ do
  it "should respect the LUA_BITS environment variable" $ do
    e <- fromMaybe Lua64 <$> expectedBits
    luaBits `shouldBe` e

  describe "maxint" $ do
    it "should be an integer" $ do
      t <- runLua $ do
        "a" `bind` maxint @Integer
        return' "math.type(a)"
      t `shouldBe` ("integer" :: String)

    it "should overflow to minint" $ do
      i <- runLua $ do
        "a" `bind` maxint @Integer
        return' "a + 1"
      i `shouldBe` minint @Integer

  describe "minint" $ do
    it "should be an integer" $ do
      t <- runLua $ do
        "b" `bind` maxint @Integer
        return' "math.type(b)"
      t `shouldBe` ("integer" :: String)

    it "should underflow to maxint" $ do
      i <- runLua $ do
        "b" `bind` minint @Integer
        return' "b - 1"
      i `shouldBe` maxint @Integer
