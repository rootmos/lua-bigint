module BignatSpec where

import Test.Hspec
--import Test.QuickCheck

import HsLua hiding ( Integer )

import LuaBigInt
import LuaUtils

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "M" `requireG` "bignat"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

evalAndPeek :: EvalLuaAndPeek
evalAndPeek = mkEvalAndPeek runAndPeek

spec :: Spec
spec = do
  describe "bignat.lua" $ do
    it "should load properly" $ do
      t <- runLua $ do
        OK <- dostring "return type(M)"
        peek @String top
      t `shouldBe` "table"
    it "should set the expected max and default bases" $ do
      let e :: Int = case luaBits of { Lua32 -> 15; Lua64 -> 31 }
      evalAndPeek @Integer "M.max_base" >>= flip shouldBe (2^e)
      evalAndPeek @Integer "M.default_base" >>= flip shouldBe (2^e)
