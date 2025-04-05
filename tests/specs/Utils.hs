module Utils where

import System.Environment ( lookupEnv )
import Test.QuickCheck ( verbose, property, Testable, Property, withMaxSuccess, mapSize )
import System.IO.Unsafe ( unsafePerformIO )
import Data.Function ( (&) )

digitsInBase :: Integer -> Integer -> [ Integer ]
digitsInBase _ x | x < 0 = undefined
digitsInBase _ 0 = []
digitsInBase base x = f x
  where f 0 = []
        f n = let (q, r) = quotRem n base in r:f q

evalInBase :: Integer -> [ Integer ] -> Integer
evalInBase b _ | b < 2 = undefined
evalInBase b ds = sum $ zipWith (*) ds (iterate (* b) 1)

properly :: Testable p => p -> Property
properly = v . m . s . property
 where v = unsafePerformIO (lookupEnv "QUICKCHECK_VERBOSE") & \case
             Nothing -> id
             Just "" -> id
             _ -> verbose
       m = unsafePerformIO (lookupEnv "QUICKCHECK_MAX_SUCCESS") & \case
             Nothing -> id
             Just "" -> id
             Just s -> withMaxSuccess (read s)
       s = unsafePerformIO (lookupEnv "QUICKCHECK_SIZE") & \case
             Nothing -> id
             Just "" -> id
             Just size -> mapSize (\_ -> read size)
