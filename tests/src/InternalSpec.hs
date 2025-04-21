module InternalSpec where

import Text.Printf
import Control.Monad ( forM_ )

import Test.Hspec
import Test.QuickCheck

import HsLua

import LuaBigInt
import LuaUtils
import Utils

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "I" `requireG` "internal"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

evalAndPeek :: EvalLuaAndPeek
evalAndPeek = mkEvalAndPeek runAndPeek

spec :: Spec
spec = do
  describe "internal.lua" $ do
    it "should load properly" $ do
      t <- runLua $ do
        dostring' "return type(I)"
        peek @String top
      t `shouldBe` "table"

    describe "binsearch" $ do
      let fn = "I.binsearch" :: String
          cmp :: Int -> String
          cmp y = printf "function(x) if x == %d then return 0 elseif %d < x then return -1 else return 1 end end" y y
          expr (a :: Int) (b :: Int) cmp = printf "%s(%d, %d, %s)" fn a b cmp

      forM_ [0..9] $ \y -> do
        let e = expr 0 9 (cmp y)
        it (printf "%s should evaluate to %d" e y) $
          evalAndPeek e >>= flip shouldBe y

      it "should work for an arbitrary range" $ properly $ \(a, NonNegative n) ->
        let b = a+n in
        forM_ [a..b] $ \y ->
          evalAndPeek (expr a b (cmp y)) >>= flip shouldBe y
