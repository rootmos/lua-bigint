module UtilsSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Printf
import Data.Char ( digitToInt )

import Utils
import Huge

spec :: Spec
spec = do
  describe "Utils" $ do
    describe "digitsInBase" $ do
      it "should render decimal digits" $ properly $ \Huge { getHuge = n}->
        (concat $ show <$> digitsInBase 10 n) `shouldBe` (show n)

      it "should render hexadecimal digits" $ properly $ \(Positive n) ->
        (digitsInBase 16 n) `shouldBe` (fromIntegral . digitToInt <$> printf "%x" n)

  it "(evalInBase b $ reverse $ digitsInBase b n) should be n" $ properly $ do
    forAll (arbitrary `suchThat` ((> 1) . fst)) $ \(b, Huge { getHuge = n}) ->
      (evalInBase b $ reverse $ digitsInBase b n) `shouldBe` n
