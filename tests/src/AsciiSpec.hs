module AsciiSpec where

import Data.List ( intersperse, intercalate )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import Utils
import LuaBigInt
import LuaUtils

import HsLua hiding ( Integer )

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "A" `requireG` "ascii"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

evalAndPeek :: EvalLuaAndPeek
evalAndPeek = mkEvalAndPeek runAndPeek

spec :: Spec
spec = do
  describe "ascii.lua" $ do
    it "should load properly" $ do
      t <- runLua $ do
        dostring' "return type(A)"
        peek @String top
      t `shouldBe` "table"

    describe "be_string_to_le_digits" $ do
      let fn = "A.be_string_to_le_digits" :: String

      let example (str :: String) (ds :: [Integer]) =
            context (printf "%s(\"%s\") -> %s" fn str (show ds)) $ do
              it "should decode digits properly" $ do
                (evalAndPeek $ printf "%s('%s')" fn str) >>= flip shouldBe ds

      describe "examples" $ do
        example "" []
        example "0" [0]
        example "abcdef097" [7, 9, 0, 15, 14, 13, 12, 11, 10]

      it "should work for decimals" $ properly $ \(NonNegative n) -> do
        ds <- evalAndPeek $ printf "%s('%d')" fn n
        (evalInBase 10 ds) `shouldBe` n

      it "should work for hexadecimals" $ properly $ \(NonNegative n) -> do
        ds <- evalAndPeek $ printf "%s('%x')" fn n
        (evalInBase 16 ds) `shouldBe` n

    describe "le_digits_to_be_string" $ do
      let fn = "A.le_digits_to_be_string" :: String

      let testCase (expr :: String) (str :: String) =
            evalAndPeek (printf "%s%s" fn expr) >>= flip shouldBe str

      let example (expr :: String) (str :: String) =
            context (printf "%s%s -> \"%s\"" fn expr str) $ do
              it "should encode digits properly" $ testCase expr str

      describe "examples" $ do
        example "()" ""
        example "{}" ""
        example "{0}" "0"
        example "{0,1,0}" "010"

        example "{15}" "f"
        example "{10, 15}" "fa"
        example "{0, 10, 15}" "fa0"

      let embrace = printf "{%s}"

      it "should work for decimals" $ properly $ \(Positive (n :: Integer)) -> do
        let r = intersperse ',' . reverse
        testCase (embrace . r $ show n) (show n)

      it "should work for hexadecimals" $ properly $ \(Positive n) -> do
        let ds = digitsInBase 16 n
            r = intercalate ","
        testCase (embrace . r $ show <$> ds) (toHex n)
