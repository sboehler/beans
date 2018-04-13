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
import           Haricot.AST
import           Haricot.Parser         (parseFile)
import           Haricot.Prices
import           System.Environment     (getArgs)


parse :: (MonadIO m, MonadThrow m) => m ()
parse = do
  (file:_) <- liftIO getArgs
  ast <- parseFile file
  let ledger = buildLedger ast
  let prices = calculatePrices ledger
  accountsHistory <- calculateAccounts ledger
  let accounts = M.lookupLE (fromGregorian 2017 12 9) accountsHistory
      accounts' = M.mapKeysWith mappend f . snd <$> accounts
  liftIO $ print accounts'
  where
    f (AccountName (n:_)) = n
    f (AccountName []) = ""
