module Beans.Command.Import
  ( run,
    Options (..),
  )
where

import Beans.Account (Account)
import Beans.Command (Command (..))
import qualified Beans.Import.CH.Cumulus
import qualified Beans.Import.CH.Postfinance
import qualified Beans.Import.CH.SupercardPlus
import qualified Beans.Import.CH.Swissquote
import Beans.Import.Common (Config (..), ImporterException)
import qualified Beans.Import.US.InteractiveBrokers
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as P

newtype ImportException = InvalidImporter String deriving (Show)

instance Exception ImportException

data Options
  = Options
      { importer :: Text,
        account :: Account,
        inputFile :: FilePath
      }
  deriving (Show)

run :: (MonadThrow m, MonadIO m, MonadReader Options m) => m ()
run = do
  Options {importer, account, inputFile} <- ask
  parse <- getParser importer
  bytes <- liftIO $ B.readFile inputFile
  let config = Config inputFile account
  transactions <- case parse config bytes of
    Left e -> throwM e
    Right d -> return d
  liftIO $ print . P.sep . P.punctuate P.hardline $ P.pretty <$> transactions

getParser :: MonadThrow m => Text -> m (Config -> B.ByteString -> Either ImporterException [Command])
getParser "ch.postfinance" = return Beans.Import.CH.Postfinance.parse
getParser "us.interactivebrokers" = return Beans.Import.US.InteractiveBrokers.parse
getParser "ch.supercardplus" = return Beans.Import.CH.SupercardPlus.parse
getParser "ch.swissquote" = return Beans.Import.CH.Swissquote.parse
getParser "ch.cumulus" = return Beans.Import.CH.Cumulus.parse
getParser n = throwM $ InvalidImporter $ "Invalid importer: " <> show n
