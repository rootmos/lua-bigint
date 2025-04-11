module LuaUtilsSpec where

import Test.Hspec

import LuaUtils

spec :: Spec
spec = do
  describe "LuaUtils" $ do
    it "should figure out Lua's bits" $ do
      luaBits `shouldBe` Lua64
