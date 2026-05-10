module HW where

import Control.Monad.State
import Data.Map

-- Task 1

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

-- Task 2

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq Expr Expr

eval :: Expr -> State (Map String Int) Int
eval e = case e of
  Num x -> return x
  Var name -> do
    map <- get
    return $ map ! name
  Add e1 e2 -> do
    x <- eval e1
    y <- eval e2
    return $ x + y
  Mul e1 e2 -> do
    x <- eval e1
    y <- eval e2
    return $ x * y
  Neg e -> do
    x <- eval e
    return (-x)
  Assign name e -> do
    x <- eval e
    modify (insert name x)
    return x
  Seq e1 e2 -> do
    eval e1
    eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) empty
