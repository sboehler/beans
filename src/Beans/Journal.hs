module Beans.Journal (journalCommand) where

import           Beans.Options          (JournalOptions (..))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader)

journalCommand
  :: (MonadIO m, MonadThrow m, MonadReader JournalOptions m) => m ()

journalCommand = undefined
