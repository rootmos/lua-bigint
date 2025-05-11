module Utils where

import Data.Function ( (&) )
import System.Environment ( lookupEnv )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Printf

import qualified Data.ByteString as BS

import Test.QuickCheck

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
properly = v . m . s . ns . vs . property
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
       ns = unsafePerformIO (lookupEnv "QUICKCHECK_NO_SHRINKING") & \case
             Nothing -> id
             Just "" -> id
             Just _ -> noShrinking
       vs = unsafePerformIO (lookupEnv "QUICKCHECK_VERBOSE_SHRINKING") & \case
             Nothing -> id
             Just "" -> id
             Just _ -> verboseShrinking

be :: Bool -> String
be True = "be"
be False = "not be"

toHex :: Integer -> String
toHex n = printf "%x" n

toLeBytes :: Integer -> BS.ByteString
toLeBytes i = BS.pack $ fmap fromIntegral $ digitsInBase 256 i

toBeBytes :: Integer -> BS.ByteString
toBeBytes = BS.reverse . toLeBytes
