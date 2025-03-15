module LuaBigIntSpec where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "addition" $ do
    it "should have an identity element" $ property $ \(a :: Int) ->
        a + 0 `shouldBe` a
    it "should be commutative" $ property $ \(a :: Int) b ->
        a + b `shouldBe` b + a
