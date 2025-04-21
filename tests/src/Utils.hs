module Utils where

import Data.Function ( (&) )
import System.Environment ( lookupEnv )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Printf

import Test.QuickCheck ( verbose
                       , property
                       , Testable
                       , Property
                       , withMaxSuccess
                       , mapSize
                       , Gen
                       , chooseInt
                       , chooseInteger
                       , infiniteListOf
                       )

digitsInBase :: Integer -> Integer -> [ Integer ]
digitsInBase _ x | x < 0 = undefined
digitsInBase _ 0 = []
digitsInBase base x = f x
  where f 0 = []
        f n = let (q, r) = quotRem n base in r:f q

evalInBase :: Integer -> [ Integer ] -> Integer
evalInBase b _ | b < 2 = undefined
evalInBase b ds = sum $ zipWith (*) ds (iterate (* b) 1)

genDigits :: Integer -> Int -> Gen [ Integer ]
genDigits base n =
  fmap (take n) $ infiniteListOf $ chooseInteger (0, base - 1)

genDigitsWithLeadingZeroes :: Integer -> Int -> Gen [ Integer ]
genDigitsWithLeadingZeroes base n = do
  m <- chooseInt (0, n)
  (++) (take (n - m) $ repeat 0) <$> genDigits base m

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

be :: Bool -> String
be True = "be"
be False = "not be"

toHex :: Integer -> String
toHex n = printf "%x" n
