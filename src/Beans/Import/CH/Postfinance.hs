module Beans.Import.CH.Postfinance where

import           Beans.Data.Accounts        (Amount, CommodityName (..))
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString.Lazy       as BL
import           Data.Monoid                (Sum (Sum))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import           Data.Text.Lazy.Encoding    (decodeLatin1)
import           Data.Time.Calendar         (Day, fromGregorian)
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec, count, eof, manyTill,
                                             optional, parse, skipManyTill,
                                             some, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, anyChar, char,
                                             digitChar, eol)
import qualified Text.Megaparsec.Char.Lexer as L

data PostfinanceData = PostfinanceData
  { _currency :: CommodityName
  , _entries  :: [Entry]
  } deriving (Eq, Show)

data Entry = Entry
  { _bookingDate :: Day
  , _description :: Text
  , _amount      :: Amount
  , _valueDate   :: Day
  , _balance     :: Maybe Amount
  } deriving (Eq, Show)

readCSV :: (MonadIO m, MonadThrow m) => FilePath -> m PostfinanceData
readCSV f = do
  source <- liftIO $ decodeLatin1 <$> BL.readFile f
  case parse postfinanceData mempty source of
    Left e  -> throwM e
    Right d -> return d

type Parser = Parsec Void Text

postfinanceData :: Parser PostfinanceData
postfinanceData =
  PostfinanceData <$> (count 4 ignoreLine >> ignoreField >> currency) <*>
  (ignoreLine >> some entry) <*
  (eol >> ignoreLine >> ignoreLine >> eof)

symbol :: Text -> Parser Text
symbol = L.symbol separator

entry :: Parser Entry
entry = Entry <$> date <*> description <*> entryAmount <*> date <*> balance

entryAmount :: Parser Amount
entryAmount = field $ credit <|> debit
    where
      debit = amount <* separator
      credit = separator *> amount

currency :: Parser CommodityName
currency = field $ CommodityName . T.toStrict . T.pack <$> some alphaNumChar

date :: Parser Day
date = field $ fromGregorian <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2)
  where
    dash = char '-'
    int n = read <$> count n digitChar

description :: Parser Text
description = quote >> T.pack <$> manyTill anyChar (quote >> separator)
  where
    quote = char '"'

amount :: Parser Amount
amount = Sum <$> L.signed mempty L.scientific

balance :: Parser (Maybe Amount)
balance = field $ optional amount

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anyChar eol

ignoreField :: Parser ()
ignoreField = void $ skipManyTill anyChar separator

separator :: Parser ()
separator = void semicolon <|> void eol
  where
    semicolon = char ';'

field :: Parser a -> Parser a
field = L.lexeme separator
