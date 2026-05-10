module HW where

import Control.Monad.State

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

popTwo :: [Int] -> (Maybe (Int, Int), [Int])
popTwo (x1 : (x2 : xs)) = (Just (x1, x2), xs)
popTwo list = (Nothing, list)

execInstr :: Instr -> State [Int] ()
execInstr i = case i of
  PUSH x -> modify (x :)
  POP -> do
    stack <- get
    case stack of
      [] -> return ()
      (x : xs) -> put xs
  DUP -> do
    stack <- get
    case stack of
      [] -> return ()
      (x : xs) -> put (x : x : xs)
  SWAP -> do
    stack <- get
    let (maybeXs, rest) = popTwo stack
    case maybeXs of
      Nothing -> return ()
      Just (x1, x2) -> put (x2 : x1 : rest)
  ADD -> do
    stack <- get
    let (maybeXs, rest) = popTwo stack
    case maybeXs of
      Nothing -> return ()
      Just (x1, x2) -> put ((x1 + x2) : rest)
  MUL -> do
    stack <- get
    let (maybeXs, rest) = popTwo stack
    case maybeXs of
      Nothing -> return ()
      Just (x1, x2) -> put ((x1 * x2) : rest)
  NEG -> do
    stack <- get
    case stack of
      [] -> return ()
      (x : xs) -> put ((-x) : xs)

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (x : xs) = do
  execInstr x
  execProg xs

runProg :: [Instr] -> [Int]
runProg program = execState (execProg program) []
