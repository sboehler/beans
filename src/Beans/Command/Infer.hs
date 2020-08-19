module Beans.Command.Infer
  ( run,
    Options (..),
  )
where

import qualified Beans.Account as Account
import Beans.Command (Command (..), Directive (..))
import qualified Beans.Infer as Infer
import Beans.Parser (directives, parseFile, parseSource)
import qualified Beans.Transaction as Transaction
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text

data Options = Options
  { infTrainingFile :: FilePath,
    infTargetFile :: FilePath
  }
  deriving (Show)

run :: (MonadThrow m, MonadReader Options m, MonadIO m) => m ()
run = do
  Options {infTrainingFile, infTargetFile} <- Reader.ask
  trainingSet <- parseFile infTrainingFile
  oldText <- Text.decodeUtf8 <$> (liftIO $ BS.readFile infTargetFile)
  candidates <- parseSource directives infTargetFile oldText
  let transactions = do
        CmdDirective _ (CmdTransaction t) <- trainingSet
        Reader.guard (Account.unknown `notElem` Transaction.accounts t)
        pure t
      model = Infer.train transactions
      fixed = fmap (Infer.fixDirective model) candidates
      newText = Infer.updateDirectives fixed oldText
  liftIO $ BS.writeFile infTargetFile $ Text.encodeUtf8 newText
