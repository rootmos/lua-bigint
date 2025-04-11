module LuaUtilsSpec where

import System.Environment ( lookupEnv )
import Data.Maybe ( fromMaybe )
import Test.Hspec

import LuaUtils

expectedBits :: IO (Maybe LuaBits)
expectedBits = f <$> lookupEnv "LUA_BITS"
  where f (Just "32") = Just Lua32
        f (Just "64") = Just Lua64
        f (Just "") = Nothing
        f Nothing = Nothing
        f (Just s) = error $ "unable to parse LUA_BITS: " ++ s

spec :: Spec
spec = do
  describe "LuaUtils" $ do
    it "should respect the LUA_BITS environment variable" $ do
      e <- fromMaybe Lua64 <$> expectedBits
      luaBits `shouldBe` e
