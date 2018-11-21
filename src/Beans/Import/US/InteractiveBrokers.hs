module Beans.Import.US.InteractiveBrokers
  ( name
  , parseEntries
  )
where

import           Prelude                           hiding ( unwords )
import           Data.Char                                ( isAlphaNum )
import           Data.List                                ( sort )
import           Data.Text                                ( Text
                                                          , pack
                                                          , unwords
                                                          , toLower
                                                          )
import           Data.Monoid                              ( Sum(..) )
import           Control.Monad                            ( void )
import           Control.Monad.Catch                      ( MonadThrow
                                                          , throwM
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )

import           Beans.Import.Common                      ( Context(..)
                                                          , ImporterException(..)
                                                          , Config(..)
                                                          )
import qualified Data.ByteString               as B
import           Data.Text.Encoding                       ( decodeLatin1 )

import qualified Beans.Data.Map                as M
import qualified Text.Megaparsec.Char.Lexer    as L
import           Beans.Model                              ( Commodity(..)
                                                          , Dated(Dated)
                                                          , Flag(Complete)
                                                          , Command(Transaction)
                                                          , Date
                                                          , fromGreg
                                                          , Lot(Lot)
                                                          , Account
                                                          , Position(Position)
                                                          , Amount
                                                          )
import           Text.Megaparsec.Char                     ( alphaNumChar
                                                          , char
                                                          , anyChar
                                                          , space
                                                          , string
                                                          , digitChar
                                                          , eol
                                                          )
import           Control.Exception                        ( Exception )
import           Text.Megaparsec                          ( Parsec
                                                          , ShowErrorComponent
                                                          , showErrorComponent
                                                          , customFailure
                                                          , count
                                                          , skipManyTill
                                                          , parse
                                                          , parseErrorPretty
                                                          , takeWhile1P
                                                          , some
                                                          , many
                                                          , try
                                                          , (<|>)
                                                          )
import           Data.Maybe                               ( catMaybes )
import           Control.Monad.Reader                     ( ReaderT
                                                          , asks
                                                          , MonadReader
                                                          , ask
                                                          , runReaderT
                                                          )



type Parser = ReaderT Config (Parsec ParserException Text)

-- parser exception
newtype ParserException =
  AccountNotFound Text
  deriving (Eq, Show, Ord)

instance Exception ParserException

instance ShowErrorComponent ParserException where
  showErrorComponent (AccountNotFound t) =
    "Account not found: " ++ show t


name :: Text
name = "us.interactivebrokers"

parseEntries
  :: (MonadIO m, MonadThrow m, MonadReader Config m) => m [Dated Command]
parseEntries = do
  c@Config { cFile } <- ask
  source             <- liftIO $ decodeLatin1 <$> B.readFile cFile
  case parse (runReaderT (many line) c) mempty source of
    Left  e -> (throwM . ImporterException . parseErrorPretty) e
    Right d -> sort . catMaybes <$> return d


line :: Parser (Maybe (Dated Command))
line =
  Just
    <$> (   try depositWithdrawalOrFee
        <|> try trade
        <|> try dividendOrWithholdingTax
        )
    <|> (skipLine >> pure Nothing)

askAccount :: Context -> Parser Account
askAccount entry = do
  evaluator <- asks cEvaluator
  case evaluator entry of
    Just account -> return account
    Nothing      -> customFailure $ AccountNotFound $ pack . show $ entry


depositWithdrawalOrFee :: Parser (Dated Command)
depositWithdrawalOrFee = do
  t <-
    try (cField "Deposits & Withdrawals" <* cField "Data")
    <|> try (cField "Interest" <* cField "Data")
    <|> try (cField "Fees" <* (cField "Data" >> cField "Other Fees"))
  currency    <- commodityField
  date        <- dateField
  description <- textField
  amount      <- amountField <* skipRestOfLine
  account     <- asks cAccount
  other       <- askAccount
    $ Context date (toLower t) description amount currency name
  let bookings = M.fromListM
        [ (Position account currency Nothing, M.singleton currency amount)
        , (Position other currency Nothing  , M.singleton currency (-amount))
        ]
  return $ Dated date $ Transaction Complete
                                    (unwords [t, "-" :: Text, description])
                                    []
                                    bookings

trade :: Parser (Dated Command)
trade = do
  category <- cField "Trades" >> cField "Data" >> cField "Order" >> textField
  currency       <- commodityField
  symbol         <- commodityField
  date           <- dateTimeField
  amount         <- amountField
  price          <- amountField <* skipField
  purchaseAmount <- amountField
  feeAmount      <- amountField <* skipRestOfLine
  account        <- asks cAccount
  feeAccount <- askAccount $ Context date "fee" category feeAmount currency name
  let
    lot      = Lot price currency date Nothing
    bookings = M.fromListM
      [ (Position account symbol (Just lot), M.singleton symbol amount)
      , (Position account currency Nothing, M.singleton currency purchaseAmount)
      , (Position feeAccount currency Nothing, M.singleton currency feeAmount)
      ]
  return $ Dated date $ Transaction Complete category [] bookings

dividendOrWithholdingTax :: Parser (Dated Command)
dividendOrWithholdingTax = do
  t        <- cField "Dividends" <|> cField "Withholding Tax"
  currency <- cField "Data" >> commodityField
  date     <- dateField
  --description     <- textField
  symbol   <-
    Commodity <$> takeWhile1P (Just "Commodity name") isAlphaNum <* space
  isin <-
    char '(' >> takeWhile1P (Just "ISIN") isAlphaNum <* (char ')' >> space)
  description <- field $ takeWhile1P Nothing (/= ',')
  amount      <- amountField
  account     <- asks cAccount
  let desc = unwords [t, (pack . show) symbol, isin, description]
  dividendAccount <- askAccount
    $ Context date (toLower t) desc amount currency name
  let
    bookings = M.fromListM
      [ (Position account currency Nothing, M.singleton currency amount)
      , (Position account symbol Nothing  , M.singleton symbol 0)
      , ( Position dividendAccount currency Nothing
        , M.singleton currency (-amount)
        )
      ]
  return $ Dated date $ Transaction Complete desc [] bookings

textField :: Parser Text
textField = field $ takeWhile1P Nothing (/= ',')

cField :: Text -> Parser Text
cField = field . string

dateField :: Parser Date
dateField = field (fromGreg <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
 where
  dash = char '-'
  int n = read <$> count n digitChar

dateTimeField :: Parser Date
dateTimeField =
  field
    $  quote
    >> (fromGreg <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
    <* skipManyTill anyChar quote
 where
  dash  = char '-'
  quote = char '"'
  int n = read <$> count n digitChar

commodityField :: Parser Commodity
commodityField = field $ Commodity . pack <$> some alphaNumChar

amountField :: Parser Amount
amountField = field $ Sum <$> L.signed (pure ()) L.scientific

skipLine :: Parser ()
skipLine = void $ skipManyTill anyChar eol

skipField :: Parser ()
skipField = void $ skipManyTill anyChar separator

skipRestOfLine :: Parser ()
skipRestOfLine = void $ skipManyTill anyChar eol

separator :: Parser ()
separator = void comma

comma :: Parser Char
comma = char ','

field :: Parser a -> Parser a
field = L.lexeme separator
