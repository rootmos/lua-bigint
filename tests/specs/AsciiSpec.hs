module AsciiSpec where

import Data.List ( intersperse, intercalate )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import LuaBigInt
import LuaUtils

import HsLua hiding ( Integer, property, concat )

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "A" `requireG` "ascii"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

evalAndPeek :: EvalLuaAndPeek
evalAndPeek = mkEvalAndPeek runAndPeek

digitsInBase :: Integer -> Integer -> [ Integer ]
digitsInBase _ x | x < 0 = undefined
digitsInBase _ 0 = [ 0 ]
digitsInBase base x = f [] x
  where f acc 0 = acc
        f acc n = let (q, r) = quotRem n base in f (r:acc) q

evalInBase :: Integer -> [ Integer ] -> Integer
evalInBase b _ | b < 2 = undefined
evalInBase b ds = sum $ zipWith (*) ds (iterate (* b) 1)

spec :: Spec
spec = do
  describe "sanity checks" $ do
   describe "digitsInBase" $
    it "should render decimals" $ property $ \(NonNegative n) ->
      (concat $ show <$> digitsInBase 10 n) `shouldBe` (show n)

   it "should survive a digitsInBase and evalInBase roundtrip" $ do
      forAll (arbitrary `suchThat` ((> 1) . fst)) $ \(b, NonNegative n) ->
        (evalInBase b $ reverse $ digitsInBase b n) `shouldBe` n

  describe "ascii.lua" $ do
    it "should load properly" $ do
      t <- runLua $ do
        OK <- dostring "return type(A)"
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

      it "should work for decimals" $ property $ \(NonNegative n) -> do
        ds <- evalAndPeek $ printf "%s('%d')" fn n
        (evalInBase 10 ds) `shouldBe` n

      it "should work for hexadecimals" $ property $ \(NonNegative n) -> do
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

      it "should work for decimals" $ property $ \(NonNegative (n :: Integer)) -> do
        let r = intersperse ',' . reverse
        testCase (embrace . r $ show n) (show n)

      it "should work for hexadecimals" $ property $ \(NonNegative (n :: Integer)) -> do
        let ds = digitsInBase 16 n
            r = intercalate "," . reverse
        testCase (embrace . r $ show <$> ds) (printf "%x" n)
