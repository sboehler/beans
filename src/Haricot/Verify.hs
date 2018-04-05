module Haricot.Holdings
  ( verify
  ) where

import           Control.Monad.Catch
import           Data.Traversable    (mapM)
import           Haricot.Accounts
import           Haricot.AST
import           Haricot.Ledger      (Timestep (..))

verify :: MonadThrow m => Accounts -> Timestep -> m [Transaction]
verify accounts Timestep {_transactions} =
  mapM (verifyTransaction accounts) _transactions

verifyTransaction :: MonadThrow m => Accounts -> Transaction -> m Transaction
verifyTransaction accounts Transaction {..} = undefined
