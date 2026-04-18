module HW where

import Control.Monad (guard)
import Data.Map

-- Task 1

type Pos = (Int, Int)

data Dir = N | S | E | W deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  moves <- maze !? pos
  moves !? dir

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = return pos
followPath maze pos (d : irs) = do
  next <- move maze pos d
  followPath maze next irs

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = return [pos]
safePath maze pos (d : irs) = do
  next <- move maze pos d
  rest <- safePath maze next irs
  return (pos : rest)

-- Task 2

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse decode
  where
    decode c = do
      key !? c

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse decode
  where
    decode string = do
      decrypt key string

-- Task 3

type Guest = String

type Conflict = (Guest, Guest)

-- Note: I have a very strong feeling that this could be written in a vastly
-- simpler way. But it works, so I guess there's no point in rewriting, right?
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = Prelude.map snd (go [] guests)
  where
    -- we use (Guest, [Guest]) instead of [Guest] here since the table is round
    -- we always add new elements to the beginning of the list, so it's quite
    -- handy to keep the last element in memory
    -- the second argument is a list of guests without seats
    go :: [(Guest, [Guest])] -> [Guest] -> [(Guest, [Guest])]
    -- create initial 1-sized seatings
    go [] guests = do
      g <- guests
      go [(g, [g])] (Prelude.filter (/= g) guests)
    -- all guests have their place, make last check for the last and first guest
    go perms [] = do
      (firstG, g : gs) <- perms
      guard ((firstG, g) `notElem` conflicts)
      guard ((g, firstG) `notElem` conflicts)
      perms
    -- actual computation of any other case than the first and last
    go perms guests = do
      (g, first : rest) <- perms
      next <- guests
      guard (first /= next)
      guard ((first, next) `notElem` conflicts)
      guard ((next, first) `notElem` conflicts)
      go [(g, next : first : rest)] (Prelude.filter (/= next) guests)

-- Task 4

data Result a = Failure String | Success a [String] deriving (Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Failure msg) = Failure msg
  fmap f (Success val warns) = Success (f val) warns

instance Applicative Result where
  pure val = Success val []
  liftA2 :: (a -> b -> c) -> Result a -> Result b -> Result c
  liftA2 _ (Failure msg) _ = Failure msg
  liftA2 _ _ (Failure msg) = Failure msg
  liftA2 f (Success vA warnA) (Success vB warnB) =
    Success (f vA vB) (warnA ++ warnB)

instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Failure msg) >>= _ = Failure msg
  (Success x warnx) >>= f = liftA2 (\_ y -> y) (Success x warnx) (f x)

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge age
  | age > 150 = do
      warn "Age above 150"
      return age
  | age < 0 = do
      failure "Age below 0"
  | otherwise = do
      return age

-- This reports a failure when one of the ages fails. That was the intention of
-- the task, right? I wasn't 100% sure from the description.
validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge
