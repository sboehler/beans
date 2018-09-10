module Beans.Lib
  ( runBeans
  , Command(..)
  ) where

import           Beans.Balance          (balanceCommand)
import           Beans.Import           (importCommand)
import           Beans.Options          (Command (..))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ask, runReaderT)

runBeans :: (MonadIO m, MonadThrow m) => Command -> m ()
runBeans = runReaderT run

run :: (MonadIO m, MonadThrow m, MonadReader Command m) => m ()
run = do
  command <- ask
  case command of
    Balance options -> runReaderT balanceCommand options
    Import options  -> runReaderT importCommand options
