
module Libraries (isPrime) where

primeNumbers :: Int -> [Int]
primeNumbers n = getPrimes [2..]
  where getPrimes list@(x:xs) = if x > n 
                                then [] 
                                else filterPrimes list
        filterPrimes (x:xs) = x : getPrimes (filter ((/= 0) . (`mod` x)) xs)

isPrime :: Int -> Bool
isPrime n = n `elem` primeNumbers n