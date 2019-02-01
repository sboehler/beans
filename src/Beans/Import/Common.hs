module Beans.Import.Common
  ( ImporterException(..)
  , Context(..)
  , Config(..)
  , Parser
  , askAccount
  , parseCommands
  )
where

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
                                                , errorBundlePretty
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , runReaderT
                                                , MonadReader
                                                )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import qualified Data.List                     as List


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

askAccount :: (MonadReader Config m) => Context -> m Account
askAccount entry = do
  evaluator <- asks _configEvaluator
  case evaluator entry of
    Just account -> return account
    Nothing      -> fail $ "Account not found: " <> show entry

parseCommands
  :: Config
  -> Parser [Dated Command]
  -> Text
  -> Either ImporterException [Dated Command]
parseCommands config parser input = do
  let parser' = runReaderT parser config
  case List.sort <$> parse parser' (_configFile config) input of
    Left  e -> (Left . ImporterException . errorBundlePretty) e
    Right d -> Right d
