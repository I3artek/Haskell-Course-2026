module HW where

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
