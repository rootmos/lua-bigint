module AsciiSpec where

import Test.Hspec

import LuaBigInt
import LuaUtils

import HsLua hiding ( property )

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "A" `requireG` "ascii"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

spec :: Spec
spec = do
  describe "ascii.lua" $ do
    it "should prepare properly" $ do
      t <- runLua $ do
        OK <- dostring "return type(A)"
        peek @String top
      t `shouldBe` "table"
