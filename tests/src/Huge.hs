module Huge where

import Data.List ( foldl' )

import Test.QuickCheck

data Huge = Huge { getHuge :: Integer, factors :: [ Positive Integer ] }

mkHuge :: [ Positive Integer ] -> Huge
mkHuge ns = Huge (foldl' lcg 1 $ getPositive <$> ns) ns
  where lcg a b = 7*a + 13*b

instance Show Huge where
  show (Huge n _) = show n

instance Arbitrary Huge where
  arbitrary = mkHuge <$> arbitrary
