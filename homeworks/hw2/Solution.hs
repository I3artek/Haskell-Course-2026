module HW where

import GHC.Base (divInt)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving (Show)

s1 :: Sequence Int
s1 = Append (Append (Single 2) (Single 4)) (Append (Single 5) (Single 6))

-- Task 1

instance Functor Sequence where
  fmap _ Empty = Empty
  fmap f (Single x) = Single (f x)
  fmap f (Append x y) = Append (f <$> x) (f <$> y)

-- Task 2

instance Foldable Sequence where
  foldMap _ Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Append x y) = foldMap f x <> foldMap f y

-- I'm not sure what library function should I use for these
seqToList :: Sequence a -> [a]
seqToList = foldMap (: [])

seqLength :: Sequence a -> Int
seqLength = length

-- Task 3

instance Semigroup (Sequence a) where
  (<>) = Append

instance Monoid (Sequence a) where
  mempty = Empty

-- Task 4

tailElem :: (Eq a) => a -> Sequence a -> Bool
tailElem _ Empty = False
tailElem x (Single y) = x == y
tailElem x ys = go ys []
  where
    go Empty (s : tack) = go s tack
    go Empty [] = False
    go (Single y) (s : tack) = (y == x) || go s tack
    go (Single y) [] = y == x
    go (Append ys zs) stack = go ys (zs : stack)

tailElem2 :: (Eq a) => a -> Sequence a -> Bool
tailElem2 _ Empty = False
tailElem2 x (Single y) = x == y
tailElem2 x ys = go ys []
  where
    go Empty (s : tack) = go s tack
    go (Single y) (s : tack) = (y == x) || go s tack
    go (Append ys zs) stack = go ys (zs : stack)
    go s [] = tailElem2 x s

-- Task 5

tailToList :: Sequence a -> [a]
tailToList Empty = []
tailToList (Single x) = [x]
tailToList ys = go ys []
  where
    go Empty (s : tack) = go s tack
    go (Single x) (s : tack) = x : go s tack
    go (Append ys zs) stack = go ys (zs : stack)
    go s [] = tailToList s

-- Another task 5

data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN [] = Nothing
tailRPN list = go list []
  where
    go (TAdd : os) (x : y : rest) = go os ((x + y) : rest)
    go (TSub : os) (x : y : rest) = go os ((x - y) : rest)
    go (TMul : os) (x : y : rest) = go os ((x * y) : rest)
    go (TDiv : os) (x : 0 : rest) = Nothing
    go (TDiv : os) (x : y : rest) = go os ((x `divInt` y) : rest)
    go ((TNum x) : os) stack = go os (x : stack)
    go [] [x] = Just x
    go _ _ = Nothing

-- Task 6

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myReverse :: [a] -> [a]
myReverse list = foldl (\l e -> e : l) [] list

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x l -> if p x then x : l else []) []

decimal :: [Int] -> Int
decimal = foldl (\s e -> s * 10 + e) 0

-- Task 7
countOrAdd :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
countOrAdd x [] = [(x, 1)]
countOrAdd x (e@(y, c) : es) = if x == y then (y, c + 1) : es else (x, 1) : e : es

encode :: (Eq a) => [a] -> [(a, Int)]
encode = foldr countOrAdd []

decode :: [(a, Int)] -> [a]
decode = foldr (\(e, c) l -> (replicate c e) ++ l) []
