module Beans.Import.CH.Postfinance
  ( parseEntries
  , name
  )
where

import           Debug.Trace
import           Data.Group                               ( invert )
import           Beans.Data.Directives                    ( Dated(Dated)
                                                          , Command(Transaction)
                                                          , Flag(Complete)
                                                          )
import qualified Beans.Data.Map                as M
import           Beans.Data.Accounts                      ( Amount
                                                          , Commodity(Commodity)
                                                          , Position(Position)
                                                          , Date(Date)
                                                          )
import           Beans.Import.Common                      ( Entry(..)
                                                          , Config(..)
                                                          , ImporterException(..)
                                                          )
import           Control.Monad                            ( void )
import           Control.Monad.Reader                     ( ReaderT
                                                          , asks
                                                          , MonadReader
                                                          , ask
                                                          , runReaderT
                                                          )
import           Control.Monad.Catch                      ( MonadThrow
                                                          , throwM
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import qualified Data.ByteString               as BS
import           Data.Monoid                              ( Sum(Sum) )
import           Data.Text                                ( Text
                                                          , pack
                                                          )
import qualified Data.Text                     as T
import           Data.Text.Encoding                       ( decodeLatin1 )
import           Data.Time.Calendar                       ( fromGregorian )
import           Text.Megaparsec                          ( Parsec
                                                          , ShowErrorComponent(..)
                                                          , count
                                                          , manyTill
                                                          , parse
                                                          , parseErrorPretty
                                                          , customFailure
                                                          , skipManyTill
                                                          , some
                                                          , (<|>)
                                                          )
import           Text.Megaparsec.Char                     ( alphaNumChar
                                                          , anyChar
                                                          , char
                                                          , digitChar
                                                          , eol
                                                          )
import           Control.Exception                        ( Exception )
import qualified Text.Megaparsec.Char.Lexer    as L


name :: Text
name = "ch.postfinance" :: Text

parseEntries
  :: (MonadIO m, MonadThrow m, MonadReader Config m) => m [Dated Command]
parseEntries = do
  c@Config { cFile } <- ask
  source             <- liftIO $ decodeLatin1 <$> BS.readFile cFile
  case parse (runReaderT postfinanceData c) cFile source of
    Left  e -> (throwM . ImporterException . parseErrorPretty) e
    Right d -> return d

type Parser = ReaderT Config (Parsec ParserException Text)

-- parser exception
newtype ParserException =
  AccountNotFound Text
  deriving (Eq, Show, Ord)

instance Exception ParserException

instance ShowErrorComponent ParserException where
  showErrorComponent (AccountNotFound t) =
    "Account not found: " ++ show t


postfinanceData :: Parser [Dated Command]
postfinanceData = do
  commodity <- count 3 ignoreLine >> ignoreField >> currency
  commands  <- ignoreLine >> some (command commodity)
  _         <- eol >> ignoreLine >> ignoreLine
  return commands

command :: Commodity -> Parser (Dated Command)
command commodity = do
  d         <- date
  desc      <- description
  amt       <- entryAmount
  account   <- asks cAccount
  evaluator <- asks cEvaluator
  _         <- ignoreField >> ignoreField
  let entry        = Entry d "Expense" desc (invert amt) commodity name
      otherAccount = evaluator entry
  case otherAccount of
    Nothing -> customFailure $ AccountNotFound $ pack . traceId . show $ entry
    Just a ->
      let bookings = M.fromListM
            [ (Position account commodity Nothing, M.singleton commodity amt)
            , (Position a commodity Nothing, M.singleton commodity (invert amt))
            ]
      in  return $ Dated d $ Transaction Complete desc [] bookings

entryAmount :: Parser Amount
entryAmount = field $ credit <|> debit
 where
  debit  = amount <* separator
  credit = separator *> amount

currency :: Parser Commodity
currency = field $ Commodity . T.pack <$> some alphaNumChar

date :: Parser Date
date = Date
  <$> field (fromGregorian <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
 where
  dash = char '-'
  int n = read <$> count n digitChar

description :: Parser Text
description = quote >> T.pack <$> manyTill anyChar (quote >> separator)
  where quote = char '"'

amount :: Parser Amount
amount = Sum <$> L.signed (pure ()) L.scientific

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anyChar eol

ignoreField :: Parser ()
ignoreField = void $ skipManyTill anyChar separator

separator :: Parser ()
separator = void semicolon <|> void eol where semicolon = char ';'

field :: Parser a -> Parser a
field = L.lexeme separator
