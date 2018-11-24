module Beans.Import
  ( importCommand
  )
where

import           Beans.Model                    ( Command(..)
                                                , Dated(..)
                                                )
import qualified Beans.Import.CH.Postfinance
import qualified Beans.Import.US.InteractiveBrokers

import           Beans.Import.Common            ( Config(..) )
import           Beans.Import.DSL               ( Rule
                                                , Context(..)
                                                , evaluate
                                                , Context
                                                , parseFile
                                                )
import           Beans.Options                  ( ImportOptions(..) )
import           Beans.Pretty                   ( )
import           Data.Text                      ( Text )
import           Control.Exception              ( Exception )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , runReaderT
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text.Prettyprint.Doc     as P

data ImportException = NoAccountFound [Rule] Context | InvalidImporter String deriving (Show)

instance Exception ImportException

importCommand :: (MonadThrow m, MonadIO m) => ImportOptions -> m ()
importCommand ImportOptions {..} = do
  parse        <- getParser impOptImporter
  rules        <- parseFile impOptConfig
  transactions <- runReaderT
    parse
    (Config (evaluate rules) impOptData impOptAccount)
  liftIO $ print $ P.sep (P.pretty <$> transactions)

getParser
  :: (MonadThrow n, MonadIO m, MonadThrow m, MonadReader Config m)
  => Text
  -> n (m [Dated Command])
getParser n
  | n == Beans.Import.CH.Postfinance.name = return
    Beans.Import.CH.Postfinance.parse
  | n == Beans.Import.US.InteractiveBrokers.name = return
    Beans.Import.US.InteractiveBrokers.parse
  | otherwise = throwM $ InvalidImporter $ "Invalid importer: " <> show n
