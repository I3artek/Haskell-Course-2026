module HW where

import Control.Monad.State

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

pop2Apply :: (Int -> Int -> [Int]) -> State [Int] ()
pop2Apply f = do
  stack <- get
  case stack of
    (x1 : (x2 : xs)) -> put (f x1 x2 ++ xs)
    _ -> return ()

pop1Apply :: (Int -> [Int]) -> State [Int] ()
pop1Apply f = do
  stack <- get
  case stack of
    (x : xs) -> put (f x ++ xs)
    _ -> return ()

execInstr :: Instr -> State [Int] ()
execInstr i = case i of
  PUSH x -> modify (x :)
  POP -> pop1Apply $ const []
  DUP -> pop1Apply $ \x -> [x, x]
  SWAP -> pop2Apply (\x y -> [y, x])
  ADD -> pop2Apply (\x y -> [x + y])
  MUL -> pop2Apply (\x y -> [x * y])
  NEG -> pop1Apply (\x -> [-x])

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (x : xs) = do
  execInstr x
  execProg xs

runProg :: [Instr] -> [Int]
runProg program = execState (execProg program) []
