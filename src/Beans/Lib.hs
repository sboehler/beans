module Beans.Lib
  ( run,
    Command (..),
  )
where

import qualified Beans.Command.Balance as Balance
import qualified Beans.Command.Fetch as Fetch
import qualified Beans.Command.Import as Import
import qualified Beans.Command.Infer as Infer
import qualified Beans.Command.Transcode as Transcode
import Beans.Options (Command (..))
import Beans.Parser (ParserException)
import Beans.Process (ProcessException)
import Control.Monad.Catch (MonadThrow, catch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)

run :: Command -> IO ()
run c =
  run' c
    `catch` (\(e :: ProcessException) -> print $ show e)
    `catch` (\(e :: ParserException) -> print $ show e)

run' :: (MonadIO m, MonadThrow m) => Command -> m ()
run' (Balance options) = runReaderT Balance.run options
run' (Fetch options) = runReaderT Fetch.run options
run' (Import options) = runReaderT Import.run options
run' (Infer options) = runReaderT Infer.run options
run' (Transcode options) = runReaderT Transcode.run options
