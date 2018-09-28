module Beans.Import.CH.Postfinance
  ( readCSV
  ) where

import           Beans.Data.Accounts        (Amount,
                                             CommodityName (CommodityName))
import           Beans.Import.Common        (Entry (..), ImporterException (..),
                                             TransactionData (..))
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString            as BS
import           Data.Monoid                (Sum (Sum))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeLatin1)
import           Data.Time.Calendar         (Day, fromGregorian)
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec, count, manyTill, optional,
                                             parse, parseErrorPretty,
                                             skipManyTill, some, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, anyChar, char,
                                             digitChar, eol)
import qualified Text.Megaparsec.Char.Lexer as L

readCSV :: (MonadIO m, MonadThrow m) => FilePath -> m TransactionData
readCSV f = do
  source <- liftIO $ decodeLatin1 <$> BS.readFile f
  case parse postfinanceData mempty source of
    Left  e -> (throwM . ImporterException . parseErrorPretty) e
    Right d -> return d

type Parser = Parsec Void Text

postfinanceData :: Parser TransactionData
postfinanceData =
  TransactionData
    <$> (count 4 ignoreLine >> ignoreField >> currency)
    <*> (ignoreLine >> some entry)
    <*  (eol >> ignoreLine >> ignoreLine)

entry :: Parser Entry
entry = Entry <$> date <*> description <*> entryAmount <*> date <*> balance

entryAmount :: Parser Amount
entryAmount = field $ credit <|> debit
 where
  debit  = amount <* separator
  credit = separator *> amount

currency :: Parser CommodityName
currency = field $ CommodityName . T.pack <$> some alphaNumChar

date :: Parser Day
date = field $ fromGregorian <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2)
 where
  dash = char '-'
  int n = read <$> count n digitChar

description :: Parser Text
description = quote >> T.pack <$> manyTill anyChar (quote >> separator)
  where quote = char '"'

amount :: Parser Amount
amount = Sum <$> L.signed mempty L.scientific

balance :: Parser (Maybe Amount)
balance = field $ optional amount

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anyChar eol

ignoreField :: Parser ()
ignoreField = void $ skipManyTill anyChar separator

separator :: Parser ()
separator = void semicolon <|> void eol where semicolon = char ';'

field :: Parser a -> Parser a
field = L.lexeme separator
