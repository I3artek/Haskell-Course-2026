module HW where

-- Task 1

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
  | n < 4 = []
  | otherwise = [(p, q) | p <- primes, q <- primes, p + q == n, p <= q]
  where
    primes = primesTo n

-- Task 2

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs list = [(x, y) | x <- list, y <- list, x < y, gcd x y == 1]

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

-- Task 5

-- Let's define a nice helper
applyToEachWithRest :: (a -> [a] -> [[b]]) -> [a] -> [[b]]
applyToEachWithRest _ [] = []
applyToEachWithRest f (x : xs) = go [] (x : xs)
  where
    go used [] = []
    go used (x : xs) = (f x (used ++ xs)) ++ (go (used ++ [x]) xs)

insertions :: a -> [a] -> [[a]]
insertions x [] = [[x]]
insertions y (x : xs) = go [] (x : xs)
  where
    go list [] = [list ++ [y]]
    go list (x : xs) = [list ++ [y] ++ (x : xs)] ++ (go (list ++ [x]) xs)

getElAndRest :: a -> [a] -> [[([a], [a])]]
getElAndRest x xs = [[([x], xs)]]

pairmutations :: Int -> [a] -> [([a], [a])]
pairmutations 0 list = [([], list)]
pairmutations 1 list = concat (applyToEachWithRest getElAndRest list)
pairmutations n list = [p | smallerp@(ps, ns) <- smaller, p <- concat (applyToEachWithRest (\x xs -> [[(ps ++ [x], xs)]]) ns)]
  where
    smaller = pairmutations (n - 1) list

permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations n list = map (\(x, y) -> x) (pairmutations n list)

-- Task 6

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] list = list
merge list [] = list
merge (x : xs) (y : ys)
  | x == y = merge xs (y : ys)
  | x < y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
