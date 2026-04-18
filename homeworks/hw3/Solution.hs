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
