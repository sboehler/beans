module Beans.Import (importCommand) where

import           Beans.Import.CH.Postfinance (readCSV)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.Catch        (MonadThrow)

importCommand ::
     (MonadReader () m, MonadThrow m, MonadIO m) => m ()
importCommand = do
  -- TODO: implement & remove test implementation
  d <- readCSV "/home/silvio/Downloads/export_Bewegungen_20180707.csv"
  liftIO $ print d
