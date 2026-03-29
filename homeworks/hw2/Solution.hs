module HW where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving (Show)

s1 :: Sequence Int
s1 = Append (Append (Single 2) (Single 4)) (Append (Single 5) (Single 6))

-- Task 1

instance Functor Sequence where
  fmap _ Empty = Empty
  fmap f (Single x) = Single (f x)
  fmap f (Append x y) = Append (f <$> x) (f <$> y)
