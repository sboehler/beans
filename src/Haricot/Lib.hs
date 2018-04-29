module Haricot.Lib
  ( parse
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (runReaderT)
import           Control.Monad.Trans    (liftIO)
import qualified Data.Map.Strict        as M
import           Data.Scientific        (Scientific, formatScientific, FPFormat(Fixed))
import           Data.Time.Calendar
import           Haricot.Accounts
import           Haricot.AST
import           Haricot.Ledger
import           Haricot.Parser         (parseFile)
import           Haricot.Tabular
import           Haricot.Valuation
import           System.Environment     (getArgs)


parse :: (MonadIO m, MonadThrow m) => m ()
parse = do
  (file:_) <- liftIO getArgs
  ast <- parseFile file
  let ledger = buildLedger ast
  let valuationContext =
        Config
          (CommodityName "CHF")
          (AccountName ["Equity", "Valuation"])
  valuation <- runReaderT (calculateValuation ledger) valuationContext
  valHistory <- calculateAccounts valuation

  let accounts = M.lookupLE (fromGregorian 2017 12 9) valHistory
  --accounts' = M.mapKeysWith mappend (const (AccountName ["a"])) . snd <$> accounts

  case accounts of
    Just (_, a) -> liftIO $ printAccounts a
    Nothing     -> return()

data Row = Row {
  account   :: AccountName,
  commodity :: CommodityName,
  lot       :: Lot,
  amount    :: Scientific
               } deriving (Show)

printAccounts :: Accounts -> IO()
printAccounts accounts = do
  let l = mapWithKeys Row accounts
  putStrLn $
    showTable
      [ ColDesc left "Account" left (show . account)
      , ColDesc left "Amount" right (formatScientific Fixed (Just 2) . amount)
      , ColDesc left "Commodity" left (show . commodity)
      , ColDesc left "Lot" left (show . lot)
      ]
      l
