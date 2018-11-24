module Beans.Import.Common
  ( ImporterException(..)
  , Context(..)
  , Config(..)
  , Parser
  , askAccount
  , ParserException(..)
  , parseLatin1
  )
where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Beans.Model                    ( Account
                                                , Dated
                                                , Command
                                                )
import           Control.Exception              ( Exception )
import           Beans.Import.DSL               ( Evaluator
                                                , Context(..)
                                                )
import           Text.Megaparsec                ( Parsec
                                                , ShowErrorComponent(..)
                                                , parse
                                                , parseErrorPretty
                                                , customFailure
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , ask
                                                , runReaderT
                                                , MonadReader
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Encoding             ( decodeLatin1 )

import qualified Data.ByteString               as B


newtype ImporterException =
  ImporterException String
  deriving (Eq)

instance Show ImporterException where
  show (ImporterException s) = s

instance Exception ImporterException

data Config = Config {
  cEvaluator :: Evaluator,
  cFile :: FilePath,
  cAccount :: Account
  }

type Parser = ReaderT Config (Parsec ParserException Text)

newtype ParserException =
  AccountNotFound Text
  deriving (Eq, Show, Ord)

instance Exception ParserException

instance ShowErrorComponent ParserException where
  showErrorComponent (AccountNotFound t) =
    "Account not found: " ++ show t

askAccount :: Context -> Parser Account
askAccount entry = do
  evaluator <- asks cEvaluator
  case evaluator entry of
    Just account -> return account
    Nothing      -> customFailure $ AccountNotFound $ pack . show $ entry

parseCommands
  :: (MonadIO m, MonadThrow m, MonadReader Config m)
  => Parser [Dated Command]
  -> Text
  -> m [Dated Command]
parseCommands parser source = do
  c@Config { cFile } <- ask
  case parse (runReaderT parser c) cFile source of
    Left  e -> (throwM . ImporterException . parseErrorPretty) e
    Right d -> return d

parseLatin1
  :: (MonadIO m, MonadThrow m, MonadReader Config m)
  => Parser [Dated Command]
  -> m [Dated Command]
parseLatin1 parser =
  asks cFile >>= liftIO . B.readFile >>= parseCommands parser . decodeLatin1
