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
      it "should render decimal digits" $ properly $ \Huge { getHuge = n} ->
        (concat $ show <$> digitsInBase 10 n) `shouldBe` (reverse $ show n)

      it "should render hexadecimal digits" $ properly $ \(Positive n) ->
        (digitsInBase 16 n) `shouldBe` (reverse $ fromIntegral . digitToInt <$> printf "%x" n)

      describe "examples" $ do
        let example x b ds =
              it (printf "digitsInBase %d %d should be %s" b x ds) $ do
                digitsInBase b x `shouldBe` reverse (fromIntegral . digitToInt <$> ds)

        -- https://www.wolframalpha.com/input?i=454367+in+base+3
        example 454367 3 "212002021102"
        example 454367 4 "1232323133"
        example 454367 8 "1567337"
        example 454367 12 "19ab3b"

        -- https://www.wolframalpha.com/input?i=71826901+in+base+7
        example 71826901 7 "1531342606"
        example 71826901 4 "10101333313111"
        example 71826901 8 "421776725"
        example 71826901 12 "2007a5b1"
        example 71826901 16 "447fdd5"

  it "(evalInBase b . digitsInBase b $ n) should be n" $ properly $ do
    forAll (arbitrary `suchThat` ((> 1) . fst)) $ \(b, Huge { getHuge = n}) ->
      (evalInBase b . digitsInBase b $ n) `shouldBe` n

  describe "toHex" $ do
    it "should convert 34384472 to 0x20caa58" $
      (toHex 34384472) `shouldBe` "20caa58"
