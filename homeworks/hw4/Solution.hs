module HW where

newtype Reader r a = Reader {runReader :: r -> a}

-- Task 1

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader (const x)
  liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f readerA readerB = Reader go
    where
      go r = f (runReader readerA r) (runReader readerB r)

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  readerA >>= f = Reader go
    where
      go r = runReader (f (runReader readerA r)) r

-- Task 2

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

local :: (r -> r) -> Reader r a -> Reader r a
local f r = Reader (runReader r . f)
