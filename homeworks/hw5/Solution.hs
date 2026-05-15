{-# LANGUAGE RecordWildCards #-}

module HW where

import Control.Monad.State
import Data.Map
import Text.Read (readMaybe)

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

-- Task 3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM _ _ 0 j = return j -- not point in using cache here
editDistM _ _ i 0 = return i -- not point in using cache here
editDistM xs ys i j = do
  cache <- get
  let cachedDist = cache !? (i, j)
  case cachedDist of
    Just dist -> return dist
    Nothing -> do
      if xs !! (i - 1) == ys !! (j - 1)
        then do
          dist <- editDistM xs ys (i - 1) (j - 1)
          modify (insert (i, j) dist)
          return dist
        else do
          di <- editDistM xs ys (i - 1) j
          dj <- editDistM xs ys i (j - 1)
          ij <- editDistM xs ys (i - 1) (j - 1)
          let dist = 1 + minimum [di, dj, ij]
          modify (insert (i, j) dist)
          return dist

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys i j) empty
  where
    i = length xs
    j = length ys

-- Tasks 4, 5,6

data Tile
  = Decision (Map String Tile) -- each decision has its own branch
  | Obstacle Int Tile -- decrease energy and let the player pass
  | SmallTreasure Int Tile -- increase score
  | Trap Int Tile -- decrease score
  | FinalTreasure -- end the game
  deriving (Eq)

instance Show Tile where
  show (Decision choices) = "Crossroads with " ++ show (size choices) ++ " choices"
  show (Obstacle x _) = "Obstacle costing " ++ show x ++ " energy"
  show (SmallTreasure x _) = "Small Treasure worth " ++ show x ++ " points"
  show (Trap x _) = "Trap taking away " ++ show x ++ " points"
  show FinalTreasure = "Final Treasure"

-- path contains all the tiles visited (head is the most recent one)
-- it's needed for pushing the player back
data GameState = GameState
  { path :: [Tile],
    currentTile :: Tile,
    energy :: Int,
    score :: Int,
    turn :: Int
  }

modifyScore :: Int -> GameState -> GameState
modifyScore n g@GameState {score = _score, ..} = GameState {score = _score + n, ..}

modifyEnergy :: Int -> GameState -> GameState
modifyEnergy n g@GameState {energy = _energy, ..} = GameState {energy = _energy + n, ..}

nextTurn :: GameState -> GameState
nextTurn g@GameState {turn = _turn, ..} = GameState {turn = _turn + 1, ..}

moveByOne :: Tile -> GameState -> GameState
moveByOne
  nextTile
  g@GameState
    { path = _path,
      currentTile = _currentTile,
      ..
    } =
    GameState
      { path = _currentTile : _path,
        currentTile = nextTile,
        ..
      }

type AdventureGame a = StateT GameState IO a

-- treats invalid input as a roll equal 1
getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn "Input the dice result: "
  digits <- getLine
  let maybeVal :: Maybe Int = readMaybe digits
  case maybeVal of
    Just v -> if v >= 1 && v <= 6 then return v else return 1
    Nothing -> return 1

displayGameState :: GameState -> IO ()
displayGameState game = do
  putStrLn "================================================"
  print $ "Current tile: " ++ show (currentTile game)
  print $ "Energy: " ++ show (energy game)
  print $ "Score: " ++ show (score game)
  putStrLn "================================================"
  return ()

displayChoices :: [String] -> Int -> IO ()
displayChoices [] _ = return ()
displayChoices (c : cs) n = do
  print $ show n ++ ". " ++ c
  displayChoices cs (n + 1)

-- treats invalid choice as the choosing the first option
getPlayerChoice :: [String] -> IO String
getPlayerChoice [] = return ""
getPlayerChoice choices@(c : _) = do
  displayChoices choices 0
  putStrLn "Choose your option: "
  digits <- getLine
  let maybeVal :: Maybe Int = readMaybe digits
  case maybeVal of
    Just v -> if v >= 0 && v < length choices then return $ choices !! v else return c
    Nothing -> return c

handleLocation :: AdventureGame Bool
handleLocation = do
  game <- get
  lift $ displayGameState game
  let tile = currentTile game
  case tile of
    FinalTreasure -> return True
    Decision options -> do
      let choices = keys options
      choice <- lift $ getPlayerChoice choices
      -- I assume the string is always in the map due to how
      -- getPlayerChoice works
      let nextTile = options ! choice
      modify $ moveByOne nextTile
      return False
    Obstacle x nextTile -> do
      modify $ modifyEnergy $ negate x
      modify $ moveByOne nextTile
      return False
    SmallTreasure x nextTile -> do
      modify $ modifyScore x
      modify $ moveByOne nextTile
      return False
    Trap x nextTile -> do
      modify $ modifyScore $ negate x
      modify $ moveByOne nextTile
      return False

movePlayer :: Int -> AdventureGame Int
movePlayer 0 = return 0
movePlayer x = do
  finished <- handleLocation
  game <- get
  if finished || energy game <= 0
    then
      return 1
    else do
      movePlayer (x - 1)
  return 0

makeDecision :: [String] -> AdventureGame String
makeDecision = undefined

playTurn :: AdventureGame Bool
playTurn = do
  modify nextTurn
  game <- get
  lift $ displayGameState game
  if currentTile game == FinalTreasure || energy game <= 0
    then return True
    else do
      lift $ putStrLn ("Turn " ++ show (turn game))
      diceRoll <- lift getDiceRoll
      movePlayer diceRoll
      return False

playGame :: AdventureGame ()
playGame = do
  game <- get
  turnResult <- playTurn
  if turnResult
    then do
      if currentTile game == FinalTreasure
        then
          lift $ putStrLn "You've won"
        else
          lift $ putStrLn "You've lost"
    else do
      playGame

startGame :: Tile -> Int -> IO ()
startGame map initialEnergy = do
  let state =
        GameState
          { path = [],
            currentTile = map,
            energy = initialEnergy,
            score = 0,
            turn = 0
          }
  evalStateT playGame state

bigMap :: Tile
bigMap =
  Decision
    ( fromList
        [ ( "forest",
            Obstacle
              10
              ( Decision
                  ( fromList
                      [ ( "deepForest",
                          Obstacle
                            15
                            ( SmallTreasure
                                40
                                ( Decision
                                    ( fromList
                                        [ ( "hiddenCave",
                                            Trap
                                              10
                                              (SmallTreasure 100 FinalTreasure)
                                          ),
                                          ( "ancientTree",
                                            SmallTreasure
                                              60
                                              (Obstacle 20 FinalTreasure)
                                          )
                                        ]
                                    )
                                )
                            )
                        ),
                        ( "river",
                          Trap
                            5
                            ( Decision
                                ( fromList
                                    [ ( "waterfall",
                                        SmallTreasure
                                          80
                                          (Obstacle 10 FinalTreasure)
                                      ),
                                      ( "bridge",
                                        Obstacle
                                          5
                                          (Trap 15 FinalTreasure)
                                      )
                                    ]
                                )
                            )
                        )
                      ]
                  )
              )
          ),
          ( "mountains",
            Obstacle
              20
              ( Decision
                  ( fromList
                      [ ( "mine",
                          SmallTreasure
                            30
                            ( Decision
                                ( fromList
                                    [ ( "goldTunnel",
                                        SmallTreasure 120 FinalTreasure
                                      ),
                                      ( "collapsedTunnel",
                                        Trap 40 FinalTreasure
                                      )
                                    ]
                                )
                            )
                        ),
                        ( "peak",
                          Obstacle
                            25
                            (SmallTreasure 150 FinalTreasure)
                        )
                      ]
                  )
              )
          ),
          ( "village",
            SmallTreasure
              10
              ( Decision
                  ( fromList
                      [ ( "market",
                          SmallTreasure
                            25
                            ( Decision
                                ( fromList
                                    [ ( "merchant",
                                        SmallTreasure 50 FinalTreasure
                                      ),
                                      ( "thief",
                                        Trap 35 FinalTreasure
                                      )
                                    ]
                                )
                            )
                        ),
                        ( "tavern",
                          Trap
                            5
                            ( Decision
                                ( fromList
                                    [ ( "basement",
                                        Obstacle
                                          10
                                          (SmallTreasure 90 FinalTreasure)
                                      ),
                                      ( "gamblingRoom",
                                        Trap 50 FinalTreasure
                                      )
                                    ]
                                )
                            )
                        )
                      ]
                  )
              )
          ),
          ( "castle",
            Obstacle
              30
              ( Decision
                  ( fromList
                      [ ( "throneRoom",
                          Trap
                            20
                            (SmallTreasure 200 FinalTreasure)
                        ),
                        ( "dungeon",
                          Obstacle
                            15
                            ( Decision
                                ( fromList
                                    [ ( "cell",
                                        Trap 25 FinalTreasure
                                      ),
                                      ( "secretExit",
                                        SmallTreasure
                                          70
                                          (Obstacle 10 FinalTreasure)
                                      )
                                    ]
                                )
                            )
                        )
                      ]
                  )
              )
          )
        ]
    )
