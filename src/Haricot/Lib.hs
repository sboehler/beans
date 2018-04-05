module Haricot.Lib
  ( parse
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (liftIO)
import           Haricot.Parser         (parseFile)
import           Haricot.Ledger         (buildLedger)
import           System.Environment     (getArgs)


parse :: (MonadIO m, MonadThrow m) => m ()
parse = do
  (file:_) <- liftIO getArgs
  ast <- parseFile file
  let ledger = buildLedger ast
  liftIO $ print ledger
