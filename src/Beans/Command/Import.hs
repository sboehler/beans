module Beans.Command.Import
  ( run,
    Config (..),
  )
where

import Beans.Command (Command (..))
import qualified Beans.Import.CH.Cumulus
import qualified Beans.Import.CH.Postfinance
import qualified Beans.Import.CH.SupercardPlus
import qualified Beans.Import.CH.Swissquote
import qualified Beans.Import.CH.VIAC
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

run :: (MonadThrow m, MonadIO m, MonadReader Config m) => m ()
run = do
  o@Config {importer, inputFile} <- ask
  parse <- getParser importer
  bytes <- liftIO $ B.readFile inputFile
  transactions <- case parse o bytes of
    Left e -> throwM e
    Right d -> return d
  liftIO $ print . P.sep . P.punctuate P.hardline $ P.pretty <$> transactions

getParser ::
  MonadThrow m =>
  Text ->
  m (Config -> B.ByteString -> Either ImporterException [Command])
getParser "ch.postfinance" = return Beans.Import.CH.Postfinance.parse
getParser "ch.viac" = return Beans.Import.CH.VIAC.parse
getParser "us.interactivebrokers" = return Beans.Import.US.InteractiveBrokers.parse
getParser "ch.supercardplus" = return Beans.Import.CH.SupercardPlus.parse
getParser "ch.swissquote" = return Beans.Import.CH.Swissquote.parse
getParser "ch.cumulus" = return Beans.Import.CH.Cumulus.parse
getParser n = throwM $ InvalidImporter $ "Invalid importer: " <> show n
