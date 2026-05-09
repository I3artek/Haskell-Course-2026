module HW where
import GHC.Float (double2Int, int2Double)

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

-- Task 3

data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
  conf <- ask
  let rate = interestRate conf
      bal = int2Double (balance account)
  return (double2Int (bal * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
  fee <- asks transactionFee 
  let id = accountId account
      oldBalance = balance account
      -- fee = transactionFee conf
      newBalance = oldBalance - fee
  return $ Account id newBalance

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
  min <- asks minimumBalance
  let bal = balance account
      -- min = minimumBalance conf
  return (bal >= min)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
  interest <- calculateInterest account
  newAccount <- applyTransactionFee account
  balanceOk <- checkMinimumBalance account
  return (newAccount, interest, balanceOk)

-- for testing

cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
acc = Account { accountId = "A-001", balance = 1000 }

task3Test :: (Account, Int, Bool)
task3Test = runReader (processAccount acc) cfg
task3Correct = (Account {accountId = "A-001", balance = 998}, 50, True)

