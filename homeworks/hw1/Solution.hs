module HW where

-- Task 3

primesTo :: Int -> [Int]
primesTo n = sieve [2 .. n]

sieve :: [Int] -> [Int]
sieve [] = []
sieve list = go [] list
  where
    go primes [] = reverse primes
    go primes (x : xs) = go (x : primes) [y | y <- xs, y `mod` x /= 0]

isPrime :: Int -> Bool
isPrime n = elem n (primesTo n)
