module HW where

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
