module Beans.Import.Common
  ( ImporterException(..)
  , Context(..)
  , Config(..)
  , Parser
  , askAccount
  , parseLatin1
  , parseUtf8
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
                                                , parse
                                                , parseErrorPretty
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , ask
                                                , runReaderT
                                                , MonadReader
                                                )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Data.Text.Encoding             ( decodeLatin1
                                                , decodeUtf8
                                                )

import qualified Data.ByteString               as B


newtype ImporterException =
  ImporterException String
  deriving (Eq)

instance Show ImporterException where
  show (ImporterException s) = s

instance Exception ImporterException

data Config = Config {
  _configEvaluator :: Evaluator,
  _configFile :: FilePath,
  _configAccount :: Account
  }

type Parser = ReaderT Config (Parsec Void Text)

askAccount :: Context -> Parser Account
askAccount entry = do
  evaluator <- asks _configEvaluator
  case evaluator entry of
    Just account -> return account
    Nothing      -> fail $ "Account not found: " <> show entry

parseCommands
  :: (MonadIO m, MonadThrow m, MonadReader Config m)
  => Parser [Dated Command]
  -> Text
  -> m [Dated Command]
parseCommands parser source = do
  c@Config { _configFile } <- ask
  case parse (runReaderT parser c) _configFile source of
    Left  e -> (throwM . ImporterException . parseErrorPretty) e
    Right d -> return d

parseLatin1
  :: (MonadIO m, MonadThrow m, MonadReader Config m)
  => Parser [Dated Command]
  -> m [Dated Command]
parseLatin1 parser =
  asks _configFile
    >>= liftIO
    .   B.readFile
    >>= parseCommands parser
    .   decodeLatin1

parseUtf8
  :: (MonadIO m, MonadThrow m, MonadReader Config m)
  => Parser [Dated Command]
  -> m [Dated Command]
parseUtf8 parser =
  asks _configFile >>= liftIO . B.readFile >>= parseCommands parser . decodeUtf8
