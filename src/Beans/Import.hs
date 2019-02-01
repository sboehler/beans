module Beans.Import
  ( importCommand
  )
where

import qualified Beans.Import.CH.Cumulus
import qualified Beans.Import.CH.Postfinance
import qualified Beans.Import.CH.SupercardPlus
import           Beans.Import.Common            ( Config(..)
                                                , ImporterException
                                                )
import           Beans.Import.DSL               ( Rule
                                                , Context(..)
                                                , evaluate
                                                , Context
                                                , parseFile
                                                )
import qualified Beans.Import.US.InteractiveBrokers
import           Beans.Model                    ( Command(..)
                                                , Dated(..)
                                                )
import           Beans.Options                  ( ImportOptions(..) )
import           Beans.Pretty                   ( )
import           Control.Exception              ( Exception )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.ByteString               as B
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text.Prettyprint.Doc     as P


data ImportException = NoAccountFound [Rule] Context | InvalidImporter String deriving (Show)

instance Exception ImportException

importCommand :: (MonadThrow m, MonadIO m) => ImportOptions -> m ()
importCommand ImportOptions {..} = do
  parse <- getParser impOptImporter
  rules <- parseFile impOptConfig
  bytes <- liftIO $ B.readFile impOptData
  let config = Config (evaluate rules) impOptData impOptAccount
  transactions <- case parse config bytes of
    Left  e -> throwM e
    Right d -> return d
  liftIO $ print $ P.sep (P.pretty <$> transactions)

getParser
  :: MonadThrow m
  => Text
  -> m
       (  Config
       -> B.ByteString
       -> Either ImporterException [Dated Command]
       )
getParser n
  | n == Beans.Import.CH.Postfinance.name = return
    Beans.Import.CH.Postfinance.parse
  | n == Beans.Import.US.InteractiveBrokers.name = return
    Beans.Import.US.InteractiveBrokers.parse
  | n == Beans.Import.CH.SupercardPlus.name = return
    Beans.Import.CH.SupercardPlus.parse
  | n == Beans.Import.CH.Cumulus.name = return Beans.Import.CH.Cumulus.parse
  | otherwise = throwM $ InvalidImporter $ "Invalid importer: " <> show n
