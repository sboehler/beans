module Haricot.Lib
  ( parse
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (liftIO)
import qualified Data.Map.Strict        as M
import           Data.Time.Calendar
import           Haricot.Accounts
import           Haricot.Ledger
import           Haricot.Parser         (parseFile)
import           Haricot.Prices
import           System.Environment     (getArgs)


parse :: (MonadIO m, MonadThrow m) => m ()
parse = do
  (file:_) <- liftIO getArgs
  ast <- parseFile file
  let ledger = buildLedger ast
  let prices = calculatePrices ledger
  accounts <- calculateAccounts ledger
  liftIO $ print (M.lookupLE (fromGregorian 2017 12 9) accounts)
  liftIO $ print "prices"
  liftIO $ print (M.lookupLE (fromGregorian 2017 12 9) prices)
