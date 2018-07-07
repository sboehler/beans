module Beans.Import (importCommand) where

import           Control.Monad.Reader   (MonadReader)

importCommand ::
     (MonadReader () m) => m ()
importCommand = pure ()
