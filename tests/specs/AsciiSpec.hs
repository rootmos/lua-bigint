module AsciiSpec where

import Data.List ( intersperse )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import LuaBigInt
import LuaUtils

import HsLua hiding ( Integer, property )

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
      --describe "should work for some examples" $

      it "should work for decimals" $ property $ \(n :: NonNegative Integer) -> do
        ds <- runAndPeek [ printf "return A.be_string_to_le_digits('%d')" (getNonNegative n) ]
        let m = sum $ zipWith (*) ds (iterate (* 10) 1)
        m `shouldBe` (getNonNegative n)

      it "should work for hexadecimals" $ property $ \(n :: NonNegative Integer) -> do
        ds <- runAndPeek [ printf "return A.be_string_to_le_digits('%X')" (getNonNegative n) ]
        let m = sum $ zipWith (*) ds (iterate (* 16) 1)
        m `shouldBe` (getNonNegative n)

    describe "le_digits_to_be_string" $ do
      let r = intersperse ',' . reverse
      let fn = "A.le_digits_to_be_string" :: String

      let testCase2 (expr :: String) (str :: String) =
            context (printf "%s%s -> '%s'" fn expr str) $ do
              it "should encode properly" $ do
                runAndPeek @String [ printf "return %s%s" fn expr ] >>= flip shouldBe str

      let testCase (ds :: String) (ns :: String) =
            runAndPeek @String [ printf "return %s{%s}" fn ds ] >>= flip shouldBe ns

      let testCaseC (n :: Integer) =
            let ns = show n in
            let ds = r ns in
            context (printf "%s{%s} -> %s" fn ds ns) $ do
              it "should encode properly" $ do
                testCase ds ns

      describe "examples" $ do
        testCase2 "()" ""
        testCase2 "{}" ""
        testCase2 "{0}" "0"
        testCase2 "{0,1,0}" "010"

        testCaseC 0
        testCaseC 1234567890

        testCase2 "{15}" "f"
        testCase2 "{10, 15}" "fa"
        testCase2 "{0, 10, 15}" "fa0"

      it "should work for decimals" $ property $ \(n :: NonNegative Integer) -> do
        testCase (r . show $ getNonNegative n) (show $ getNonNegative n)
