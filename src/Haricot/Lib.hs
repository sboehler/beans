module Haricot.Lib
  ( parse
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (liftIO)
import qualified Data.Map.Strict        as M
import           Data.Scientific        (FPFormat (Fixed), Scientific,
                                         formatScientific)
import           Data.Time.Calendar     (fromGregorian)
import           Haricot.Accounts       (Accounts, calculateAccounts,
                                         mapWithKeys)
import           Haricot.AST            (AccountName (..), CommodityName (..),
                                         Lot)
import           Haricot.Ledger         (buildLedger)
import           Haricot.Parser         (parseFile)
import           Haricot.Tabular        (ColDesc (..), left, right, showTable)
import           Haricot.Valuation      (calculateValuation)
import           System.Environment     (getArgs)


parse :: (MonadIO m, MonadThrow m) => m ()
parse = do
  (file:_) <- liftIO getArgs
  ledger <- buildLedger <$> parseFile file
  let target = CommodityName "CHF"
      account = AccountName ["Equity", "Valuation"]
  valuation <- calculateValuation target account ledger
  valHistory <- calculateAccounts valuation
  let accounts = M.lookupLE (fromGregorian 2017 12 9) valHistory
  case accounts of
    Just (_, a) -> liftIO $ printAccounts a
    Nothing     -> return ()

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
