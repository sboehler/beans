module Beans.Command.Infer
  ( run,
  )
where

import qualified Beans.Account as Account
import Beans.Command (Command (..), Directive (..))
import qualified Beans.Infer as Infer
import Beans.Options (InferOptions (..))
import Beans.Parser (directives, parseFile, parseSource)
import qualified Beans.Transaction as Transaction
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Data.Text.IO as TextIO

run :: (MonadThrow m, MonadReader InferOptions m, MonadIO m) => m ()
run = do
  InferOptions {infTrainingFile, infTargetFile} <- Reader.ask
  trainingSet <- parseFile infTrainingFile
  oldText <- liftIO $ TextIO.readFile infTargetFile
  candidates <- parseSource directives infTargetFile oldText
  let transactions = do
        CmdDirective _ (CmdTransaction t) <- trainingSet
        Reader.guard (Account.unknown `notElem` Transaction.accounts t)
        pure t
      model = Infer.train transactions
      fixed = fmap (Infer.fixDirective model) candidates
      newText = Infer.updateDirectives fixed oldText
  liftIO $ TextIO.writeFile infTargetFile newText
