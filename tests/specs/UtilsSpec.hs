module UtilsSpec where

import Test.Hspec
import Test.QuickCheck

import Utils

spec :: Spec
spec = do
  describe "Utils" $ do
   describe "digitsInBase" $
    it "should render decimals" $ property $ \(Positive n) ->
      (concat $ show <$> digitsInBase 10 n) `shouldBe` (show n)

   it "should survive a digitsInBase and evalInBase roundtrip" $ do
      forAll (arbitrary `suchThat` ((> 1) . fst)) $ \(b, Positive n) ->
        (evalInBase b $ reverse $ digitsInBase b n) `shouldBe` n

