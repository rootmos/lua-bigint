module Huge where

import Data.List ( foldl' )
--import Text.Printf

import Test.QuickCheck

data Huge = Huge { getHuge :: Integer, factors :: [ Positive Integer ] }

mkHuge :: [ Positive Integer ] -> Huge
mkHuge ns = Huge (foldl' lcg 0 $ getPositive <$> ns) ns
  where lcg a b = 7*a + 13*b

instance Show Huge where
  show (Huge n _) = show n
  --show (Huge n fs) = printf "%d=%s" n (show fs)

instance Arbitrary Huge where
  arbitrary = sized $ \size -> do
    n <- arbitrary
    mkHuge . take (5 + size + n) . getInfiniteList <$> arbitrary

  shrink (Huge _ []) = []
  shrink (Huge _ (_:fs)) = [ mkHuge fs ]
