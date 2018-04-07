module Haricot.Lib
  ( parse
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (liftIO)
import           Data.Foldable          (foldlM)
import qualified Data.Map.Strict        as M
import           Haricot.Accounts
import           Haricot.AST
import           Haricot.Ledger
import Haricot.Verify
import           Haricot.Parser         (parseFile)
import           Haricot.Pretty
import           System.Environment     (getArgs)


parse :: (MonadIO m, MonadThrow m) => m ()
parse = do
  (file:_) <- liftIO getArgs
  ast <- parseFile file
  ast' <- completeDirectives ast
  let ledger = buildLedger ast'
  _ <- foldlM test M.empty ledger
  return ()
  --liftIO $ print ledger


test :: (MonadIO m, MonadThrow m) => Accounts -> Timestep -> m Accounts
test accounts ts@Timestep {_date} = do
  accounts' <- updateAccounts accounts ts
  liftIO $ prettyPrintAccounts $ M.filterWithKey (\k _ -> filterAssets k) accounts
  return accounts'

filterAssets :: AccountName -> Bool
filterAssets (AccountName (a:_)) = a == "Assets"
filterAssets (AccountName [])    = False

