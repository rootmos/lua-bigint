module ArbbaseSpec where

import Data.List ( intercalate, foldl' )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import LuaBigInt
import LuaUtils

import HsLua hiding ( Integer, property )

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "M" `requireG` "arbbase"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

evalAndPeek :: EvalLuaAndPeek
evalAndPeek = mkEvalAndPeek runAndPeek

-- TODO move to utils
digitsInBase :: Integer -> Integer -> [ Integer ]
digitsInBase _ x | x < 0 = undefined
digitsInBase _ 0 = [ 0 ]
digitsInBase base x = f [] x
  where f acc 0 = acc
        f acc n = let (q, r) = quotRem n base in f (r:acc) q

--newtype Largeish = Largeish Integer

--instance Show Largeish where
  --show (Largeish n) = show n

--instance Arbitrary Largeish where
  --arbitrary = do
    --ns :: [Positive Integer] <- arbitrary
    --let n = foldl' (*) 1 $ getPositive <$> ns
    --return $ Largeish n

spec :: Spec
spec = do
  describe "arbbase.lua" $ do
    it "should load properly" $ do
      t <- runLua $ do
        OK <- dostring "return type(M)"
        peek @String top
      t `shouldBe` "table"
  describe "to_hex" $ do
      let fn = "M.to_hex" :: String
          expr dec = printf "%s{%s}" fn (intercalate "," $ fmap show dec)
          example (dec :: [Int]) (hex :: [Int]) =
            let e = expr dec in
            context (printf "%s -> %s" e (show hex)) $ do
              it "should convert a decimal digits to hexadecimal digits" $ do
                evalAndPeek e >>= flip shouldBe hex

      describe "examples" $ do
        example [] []
        example [1] [1]
        example [15] [15]
        example [16] [0, 1]
        example [8,1] [2,1]
        example [3,2,1] [11, 7]
        example [0, 9, 8, 7, 6, 5, 4, 3, 2, 1] [2, 13, 2, 0, 6, 9, 9, 4]

      it "should work for any integer" $ property $ \(Positive (n :: Integer)) ->
        let e = expr (reverse $ digitsInBase 10 n) in
        evalAndPeek e >>= flip shouldBe (reverse $ digitsInBase 16 n)

      it "should work for large integers" $ property $ \(ns :: [Positive Integer]) ->
        let n = foldl' (*) 1 $ getPositive <$> ns in
        counterexample ("n := " ++ show n) $
          let e = expr (reverse $ digitsInBase 10 n) in
          evalAndPeek e >>= flip shouldBe (reverse $ digitsInBase 16 n)

      --it "should work for large integers" $ property $ \(Largeish n) ->
        --let e = expr (reverse $ digitsInBase 10 n) in
        --evalAndPeek e >>= flip shouldBe (reverse $ digitsInBase 16 n)
