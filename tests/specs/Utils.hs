module Utils where

import System.Environment ( lookupEnv )
import Test.QuickCheck ( verbose, property, Testable, Property )
import System.IO.Unsafe ( unsafePerformIO )
import Data.Function ( (&) )

digitsInBase :: Integer -> Integer -> [ Integer ]
digitsInBase _ x | x < 0 = undefined
digitsInBase _ 0 = []
digitsInBase base x = f [] x
  where f acc 0 = acc
        f acc n = let (q, r) = quotRem n base in f (r:acc) q

evalInBase :: Integer -> [ Integer ] -> Integer
evalInBase b _ | b < 2 = undefined
evalInBase b ds = sum $ zipWith (*) ds (iterate (* b) 1)

properly :: Testable p => p -> Property
properly = unsafePerformIO (lookupEnv "VERBOSE") & \case
 Nothing -> property
 Just "" -> property
 _ -> verbose . property
