module ArbbaseSpec where

import Data.List ( intercalate )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, property, concat )

import LuaBigInt
import LuaUtils
import Utils
import Huge

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "M" `requireG` "arbbase"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

evalAndPeek :: EvalLuaAndPeek
evalAndPeek = mkEvalAndPeek runAndPeek

spec :: Spec
spec = do
  describe "arbbase.lua" $ do
    it "should load properly" $ do
      t <- runLua $ do
        OK <- dostring "return type(M)"
        peek @String top
      t `shouldBe` "table"
  describe "dec_to_hex" $ do
      let fn = "M.dec_to_hex" :: String
          expr dec = printf "%s{%s}" fn (intercalate "," $ fmap show dec)
          example (dec :: [Int]) (hex :: [Int]) =
            let e = expr dec in
            context (printf "%s -> %s" e (show hex)) $ do
              it "should convert decimal digits to hexadecimal digits" $ do
                evalAndPeek e >>= flip shouldBe hex

      describe "examples" $ do
        example [] []
        example [0] []
        example [1] [1]
        example [15] [15]
        example [16] [0, 1]
        example [8,1] [2,1]
        example [3,2,1] [11, 7]
        example [0, 9, 8, 7, 6, 5, 4, 3, 2, 1] [2, 13, 2, 0, 6, 9, 9, 4]

      it "should work for arbitrary positive integers" $ property $ \(Positive (n :: Integer)) ->
        let e = expr (digitsInBase 10 n) in
        evalAndPeek e >>= flip shouldBe (digitsInBase 16 n)

      it "should work for huge integers" $ properly $ \(Huge {getHuge = n}) ->
        let e = expr (digitsInBase 10 n) in
        evalAndPeek e >>= flip shouldBe (digitsInBase 16 n)

  describe "hex_to_dec" $ do
      let fn = "M.hex_to_dec" :: String
          expr dec = printf "%s{%s}" fn (intercalate "," $ fmap show dec)
          example (dec :: [Int]) (hex :: [Int]) =
            let e = expr dec in
            context (printf "%s -> %s" e (show hex)) $ do
              it "should convert decimal digits to hexadecimal digits" $ do
                evalAndPeek e >>= flip shouldBe hex

      describe "examples" $ do
        example [] []
        example [1] [1]
        example [10] [0,1]
        example [15] [5,1]
        example [0,1] [6,1]
        example [13, 7, 15, 8] [3, 3, 7, 6, 3]

      it "should work for arbitrary positive integers" $ property $ \(Positive (n :: Integer)) ->
        let e = expr (digitsInBase 16 n) in
        evalAndPeek e >>= flip shouldBe (digitsInBase 10 n)

      it "should work for huge integers" $ properly $ \(Huge {getHuge = n}) ->
        let e = expr (digitsInBase 16 n) in
        evalAndPeek e >>= flip shouldBe (digitsInBase 10 n)
