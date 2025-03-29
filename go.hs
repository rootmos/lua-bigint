digitsInBase :: Integer -> Integer -> [ Integer ]
digitsInBase base x = f [] x
  where f acc 0 = acc
        f acc n = let (q, r) = quotRem n base in f (r:acc) q

evalInBase base ds = sum $ zipWith (*) ds (iterate (* base) 1)

main = putStrLn . show $ digitsInBase 8 1234567890
