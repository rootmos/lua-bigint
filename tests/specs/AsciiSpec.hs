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

    describe "be_string_to_le_digits" $ do
      it "should to work for decimals" $ do
        ds <- runAndPeek @[Int] [ "return A.be_string_to_le_digits('1234567890')" ]
        ds `shouldBe` [0, 9, 8, 7, 6, 5, 4, 3, 2, 1]

    describe "le_digits_to_be_string" $ do
      it "should to work for decimals" $ do
        ds <- runAndPeek @String [ "return A.le_digits_to_be_string{0,9,8,7,6,5,4,3,2,1}" ]
        ds `shouldBe` "1234567890"
