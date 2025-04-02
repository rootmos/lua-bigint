module Utils where

digitsInBase :: Integer -> Integer -> [ Integer ]
digitsInBase _ x | x < 0 = undefined
digitsInBase _ 0 = []
digitsInBase base x = f [] x
  where f acc 0 = acc
        f acc n = let (q, r) = quotRem n base in f (r:acc) q

evalInBase :: Integer -> [ Integer ] -> Integer
evalInBase b _ | b < 2 = undefined
evalInBase b ds = sum $ zipWith (*) ds (iterate (* b) 1)
